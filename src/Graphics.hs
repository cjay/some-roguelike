{-# LANGUAGE Strict #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Graphics
  ( runGraphics
  ) where

import           Control.Concurrent       (forkIO)
import           Control.Monad
import           Foreign.Ptr              (castPtr)
import qualified Graphics.UI.GLFW         as GLFW
import           Graphics.Vulkan.Core_1_0
import           Linear.V2             (V2 (..), _x, _y)
import           Numeric.DataFrame
import           Numeric.DataFrame.IO

import           Lib.Engine.Main
import           Lib.MonadIO.Chan
import           Lib.MonadIO.MVar
import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Resource
import           Lib.Utils                (orthogonalVk, scale)
import           Lib.Vulkan.Descriptor
import           Lib.Vulkan.Device
import           Lib.Vulkan.Engine
import           Lib.Vulkan.Image
import           Lib.Vulkan.Pipeline
import           Lib.Vulkan.Presentation
import           Lib.Vulkan.RenderPass
import           Lib.Vulkan.Queue
import           Lib.Vulkan.Shader

import           ViewModel

extentToAspect :: VkExtent2D -> Float
extentToAspect extent =
  let width :: Float = fromIntegral $ getField @"width" extent
      height :: Float = fromIntegral $ getField @"height" extent
  in width / height

-- | cam pos and cam size in world coordinates -> ortho projection from z 0.1 to
--   10 excluding boundaries.
viewProjMatrix :: Vec2f -> Vec2f -> Program r Mat44f
viewProjMatrix (Vec2 x y) (Vec2 wx wy) = do
  let camPos = Vec3 x y 0
      view = translate3 (- camPos)
      proj = orthogonalVk 0.1 10 wx wy
  return $ view %* proj


loadShaders :: EngineCapability -> Program r [VkPipelineShaderStageCreateInfo]
loadShaders EngineCapability{ dev } = do
    vertSM <- auto $ shaderModuleFile dev "shaders/sprites.vert.spv"
    fragSM <- auto $ shaderModuleFile dev "shaders/triangle.frag.spv"

    shaderVert
      <- createShaderStage vertSM
            VK_SHADER_STAGE_VERTEX_BIT
            Nothing

    shaderFrag
      <- createShaderStage fragSM
            VK_SHADER_STAGE_FRAGMENT_BIT
            Nothing

    logInfo $ "Createad vertex shader module: " ++ show shaderVert
    logInfo $ "Createad fragment shader module: " ++ show shaderFrag

    return [shaderVert, shaderFrag]


makePipelineLayouts :: VkDevice -> Program r (VkDescriptorSetLayout, VkPipelineLayout)
makePipelineLayouts dev = do
  frameDSL <- auto $ createDescriptorSetLayout dev [] --[uniformBinding 0]
  -- TODO automate bind ids
  materialDSL <- auto $ createDescriptorSetLayout dev [samplerBinding 0]
  pipelineLayout <- auto $ createPipelineLayout dev
    -- descriptor set numbers 0,1,..
    [frameDSL, materialDSL]
    -- push constant ranges
    [ pushConstantRange VK_SHADER_STAGE_VERTEX_BIT 0 96
    ]

  -- (transObjMems, transObjBufs) <- unzip <$> uboCreateBuffers pdev dev transObjSize maxFramesInFlight
  -- descriptorBufferInfos <- mapM (uboBufferInfo transObjSize) transObjBufs

  -- frameDescrSets <- allocateDescriptorSetsForLayout dev descriptorPool maxFramesInFlight frameDSL

  -- forM_ (zip descriptorBufferInfos frameDescrSets) $
    -- \(bufInfo, descrSet) -> updateDescriptorSet dev descrSet 0 [bufInfo] []

  return (materialDSL, pipelineLayout)



loadAssets :: EngineCapability -> VkDescriptorSetLayout -> Program r Assets
loadAssets cap@EngineCapability { dev, descriptorPool } materialDSL = do
  let texturePaths = map ("textures/" ++) ["spritesforyou.png"]
  (textureReadyEvents, descrTextureInfos) <- auto $ unzip <$> mapM
    (createTextureInfo cap True) texturePaths

  loadEvents <- newMVar textureReadyEvents

  materialDescrSets <- allocateDescriptorSetsForLayout dev descriptorPool (length descrTextureInfos) materialDSL

  forM_ (zip descrTextureInfos materialDescrSets) $
    \(texInfo, descrSet) -> updateDescriptorSet dev descrSet 0 [] [texInfo]

  return $ Assets {..}

data Assets
  = Assets
  { loadEvents        :: MVar [QueueEvent]
  , materialDescrSets :: [VkDescriptorSet]
  }


prepareRender :: EngineCapability
              -> SwapchainInfo
              -> [VkPipelineShaderStageCreateInfo]
              -> VkPipelineLayout
              -> Program r ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)], RenderContext)
prepareRender cap@EngineCapability{..} swapInfo shaderStages pipelineLayout = do
  let SwapchainInfo { swapImgs, swapExtent, swapImgFormat } = swapInfo
  -- MSAA seems to cause edge artifacts with texture atlasses. Sampling further
  -- towards the tile center works, but requires dropping more than 1% of the
  -- outer edge for some reason.
  -- msaaSamples <- getMaxUsableSampleCount pdev
  -- TODO 1 sample not allowed by validation, need way to turn off msaa properly
  let msaaSamples = VK_SAMPLE_COUNT_1_BIT
  depthFormat <- findDepthFormat pdev

  swapImgViews <- auto $
    mapM (\image -> createImageView dev image swapImgFormat VK_IMAGE_ASPECT_COLOR_BIT 1) swapImgs
  renderPass <- auto $ createRenderPass dev swapImgFormat depthFormat msaaSamples
  pipeline
    <- auto $ createGraphicsPipeline dev swapExtent
                              [] []
                              shaderStages
                              renderPass
                              pipelineLayout
                              msaaSamples
                              True

  (colorAttSem, colorAttImgView) <- auto $ createColorAttImgView cap
                                    swapImgFormat swapExtent msaaSamples
  (depthAttSem, depthAttImgView) <- auto $ createDepthAttImgView cap
                                    swapExtent msaaSamples
  let nextSems = [ (colorAttSem, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
                 , (depthAttSem, VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT)
                 ]
  framebuffers <- mapM
    (auto
      . createFramebuffer dev renderPass swapExtent
      . framebufferAttachments colorAttImgView depthAttImgView)
    swapImgViews

  let renderContext = RenderContext { pipeline, renderPass, pipelineLayout, extent=swapExtent }
  return (framebuffers, nextSems, renderContext)



-- Id of per-material descriptor set
materialSetId :: Word32
materialSetId = 1

pushTransform :: VkCommandBuffer -> VkPipelineLayout -> Mat44f -> Program r ()
pushTransform cmdBuf pipelineLayout df =
  liftIO $ thawPinDataFrame df >>= flip withDataFramePtr
    (vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 0 64 . castPtr)

pushPos :: VkCommandBuffer -> VkPipelineLayout -> Vec2f -> Program r ()
pushPos cmdBuf pipelineLayout df =
  liftIO $ thawPinDataFrame df >>= flip withDataFramePtr
    (vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 64 8 . castPtr)

pushSize :: VkCommandBuffer -> VkPipelineLayout -> Vec2f -> Program r ()
pushSize cmdBuf pipelineLayout df =
  liftIO $ thawPinDataFrame df >>= flip withDataFramePtr
    (vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 72 8 . castPtr)

pushUVPos :: VkCommandBuffer -> VkPipelineLayout -> Vec2f -> Program r ()
pushUVPos cmdBuf pipelineLayout df =
  liftIO $ thawPinDataFrame df >>= flip withDataFramePtr
    (vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 80 8 . castPtr)

pushUVSize :: VkCommandBuffer -> VkPipelineLayout -> Vec2f -> Program r ()
pushUVSize cmdBuf pipelineLayout df =
  liftIO $ thawPinDataFrame df >>= flip withDataFramePtr
    (vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 88 8 . castPtr)


data DescrBindInfo = DescrBindInfo
  { descrSet       :: VkDescriptorSet
  , dynamicOffsets :: [Word32]
  }

bindDescrSet :: VkCommandBuffer -> VkPipelineLayout -> Word32 -> DescrBindInfo -> Program r ()
bindDescrSet cmdBuf pipelineLayout descrSetId DescrBindInfo{..} = locally $ do
  descrSetPtr <- newArrayRes [descrSet]
  let descrSetCnt = 1
  let dynOffCnt = fromIntegral $ length dynamicOffsets
  dynOffPtr <- newArrayRes dynamicOffsets
  liftIO $ vkCmdBindDescriptorSets cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout
    descrSetId descrSetCnt descrSetPtr dynOffCnt dynOffPtr


data RenderContext
  = RenderContext
  { pipeline       :: VkPipeline
  , renderPass     :: VkRenderPass
  , pipelineLayout :: VkPipelineLayout
  , extent         :: VkExtent2D
  }


myAppNewWindow :: GLFW.Window -> Program r WindowState
myAppNewWindow window = do
  keyEvents <- newChan
  let keyCallback _ key _ keyState _ =
        writeChan keyEvents $ KeyEvent key keyState
  liftIO $ GLFW.setKeyCallback window (Just keyCallback)
  return WindowState {..}

myAppMainThreadHook :: WindowState -> IO ()
myAppMainThreadHook WindowState {..} = return ()

myAppStart :: (Chan Event -> IO (MVar ViewModel, MVar ViewState)) -> WindowState -> EngineCapability -> Program r MyAppState
myAppStart startGame winState@WindowState{ keyEvents } cap@EngineCapability{ dev } = do
  shaderStages <- loadShaders cap
  (materialDSL, pipelineLayout) <- makePipelineLayouts dev
  -- TODO beware of automatic resource lifetimes when making assets dynamic
  assets <- loadAssets cap materialDSL
  renderContextVar <- newEmptyMVar
  (viewModel, viewState) <- liftIO $ startGame keyEvents
  return $ MyAppState{..}

myAppNewSwapchain :: MyAppState -> SwapchainInfo -> Program r ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)])
myAppNewSwapchain MyAppState{..} swapInfo = do
  _ <- tryTakeMVar renderContextVar
  (framebuffers, nextSems, renderContext) <- prepareRender cap swapInfo shaderStages pipelineLayout
  putMVar renderContextVar renderContext
  swapMVar viewState $ ViewState { aspectRatio = extentToAspect (extent renderContext) }
  return (framebuffers, nextSems)



recordSprite :: VkCommandBuffer -> Program r ()
recordSprite cmdBuf =
  liftIO $ vkCmdDraw cmdBuf
    6 1 0 0 -- vertex count, instance count, first vertex, first instance

myAppRecordFrame :: MyAppState -> VkCommandBuffer -> VkFramebuffer -> Program r ()
myAppRecordFrame MyAppState{..} cmdBuf framebuffer = do
  let WindowState{..} = winState

  ViewModel{..} <- readMVar viewModel
  RenderContext{..} <- readMVar renderContextVar

  let texSize grid = pushUVSize cmdBuf pipelineLayout (0.999 * grid)
      texPos grid p = pushUVPos cmdBuf pipelineLayout ((p + 0.0005) * grid)


  let renderPassBeginInfo = createRenderPassBeginInfo renderPass framebuffer extent
  withVkPtr renderPassBeginInfo $ \rpbiPtr ->
    liftIO $ vkCmdBeginRenderPass cmdBuf rpbiPtr VK_SUBPASS_CONTENTS_INLINE

  ViewState { aspectRatio } <- readMVar viewState
  let viewSize = Vec2 (aspectRatio*camHeight) camHeight
      V2 x y = camPos
    in pushTransform cmdBuf pipelineLayout =<< viewProjMatrix (Vec2 x y) viewSize

  let Assets {..} = assets
      materialBindInfo = DescrBindInfo (materialDescrSets !! 0) []
      tileGrid = recip (Vec2 8 8)
      tilePos (Vec2 x y) = pushPos cmdBuf pipelineLayout (Vec2 (realToFrac x) (realToFrac y))

  -- a bit simplistic. when hot loading assets, better filter the objects that depend on them
  events <- takeMVar loadEvents
  notDone <- filterM (fmap not . isDone) events
  let allDone = null notDone
  putMVar loadEvents notDone

  when allDone $ do
    liftIO $ vkCmdBindPipeline cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
    bindDescrSet cmdBuf pipelineLayout materialSetId materialBindInfo

    pushSize cmdBuf pipelineLayout (vec2 1 1)
    texSize tileGrid

    -- walls
    texPos tileGrid (Vec2 2 4)

    forM_ walls $ \wallPos -> do
      tilePos wallPos
      recordSprite cmdBuf

    -- enemies
    texPos tileGrid (Vec2 2 2)

    forM_ enemies $ \pos -> do
      tilePos pos
      recordSprite cmdBuf

    -- player
    texPos tileGrid (Vec2 0 0)
    tilePos playerPos
    recordSprite cmdBuf

    -- dir
    texPos tileGrid (Vec2 5 3)
    tilePos (playerPos + dirIndicator)
    recordSprite cmdBuf

  liftIO $ vkCmdEndRenderPass cmdBuf

data WindowState
  = WindowState
  { window    :: GLFW.Window
  , keyEvents :: Chan Event
  }

data MyAppState
  = MyAppState
  { shaderStages     :: [VkPipelineShaderStageCreateInfo]
  , pipelineLayout   :: VkPipelineLayout
  , cap              :: EngineCapability
  , assets           :: Assets
  , renderContextVar :: MVar RenderContext
  , winState         :: WindowState
  , viewModel        :: MVar ViewModel
  , viewState        :: MVar ViewState
  }


runGraphics :: [EngineFlag] -> (Chan Event -> IO (MVar ViewModel, MVar ViewState)) -> IO ()
runGraphics flags startGame = do
  let app = App
        { windowName = "Some Roguelike"
        , windowSize = (800, 600)
        , flags
        , syncMode = VSync
        , maxFramesInFlight = 2
        , appNewWindow = myAppNewWindow
        , appMainThreadHook = myAppMainThreadHook
        , appStart = myAppStart startGame
        , appNewSwapchain = myAppNewSwapchain
        , appRecordFrame = myAppRecordFrame
        }
  runVulkanProgram app
