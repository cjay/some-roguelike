{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Graphics
  ( runGraphics
  ) where

import           Control.Concurrent       (forkIO)
import           Control.Monad
import           Data.Bits
import qualified Data.Vector.Storable as VS
import           Data.Functor                   ( (<&>) )
import           Foreign.Ptr              (castPtr)
import qualified Graphics.UI.GLFW         as GLFW
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Linear.V2             (V2 (..), _x, _y)
import           Numeric.DataFrame
import           Numeric.DataFrame.IO
import           UnliftIO.Chan
import           UnliftIO.MVar

import           Vulkyrie.Engine.Main
import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Resource
import           Vulkyrie.Utils                (orthogonalVk, scale)
import           Vulkyrie.Vulkan.Command
import           Vulkyrie.Vulkan.Default.Pipeline
import           Vulkyrie.Vulkan.Default.RenderPass
import           Vulkyrie.Vulkan.Descriptor
import           Vulkyrie.Vulkan.Device
import           Vulkyrie.Vulkan.Engine
import           Vulkyrie.Vulkan.Framebuffer
import           Vulkyrie.Vulkan.Image
import           Vulkyrie.Vulkan.PipelineLayout
import           Vulkyrie.Vulkan.Presentation
import           Vulkyrie.Vulkan.Queue
import           Vulkyrie.Vulkan.Shader

import           ViewModel

extentToAspect :: VkExtent2D -> Float
extentToAspect extent =
  let width :: Float = fromIntegral $ getField @"width" extent
      height :: Float = fromIntegral $ getField @"height" extent
  in width / height

-- | cam pos and cam size in world coordinates -> ortho projection from z 0.1 to
--   10 excluding boundaries.
viewProjMatrix :: Vec2f -> Vec2f -> Program Mat44f
viewProjMatrix (Vec2 x y) (Vec2 wx wy) = do
  let camPos = Vec3 x y 0
      view = translate3 (- camPos)
      proj = orthogonalVk 0.1 10 wx wy
  return $ view %* proj


loadShaders :: EngineCapability -> Program [VkPipelineShaderStageCreateInfo]
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


makePipelineLayouts :: VkDevice -> Program (VkDescriptorSetLayout, VkPipelineLayout)
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

  return (materialDSL, pipelineLayout)



loadAssets :: EngineCapability -> VkDescriptorSetLayout -> Program Assets
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
              -> Program ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)], RenderContext)
prepareRender cap@EngineCapability{..} swapInfo shaderStages pipelineLayout = do
  let SwapchainInfo { swapImgs, swapExtent, swapImgFormat } = swapInfo
  -- MSAA seems to cause edge artifacts with texture atlasses. Sampling further
  -- towards the tile center works, but requires dropping more than 1% of the
  -- outer edge for some reason.
  -- msaaSamples <- getMaxUsableSampleCount pdev
  let msaaSamples = VK_SAMPLE_COUNT_1_BIT
  depthFormat <- findDepthFormat pdev

  swapImgViews <- auto $
    mapM (\image -> createImageView dev image swapImgFormat VK_IMAGE_ASPECT_COLOR_BIT 1) swapImgs
  renderPass <- auto $ createRenderPass dev swapImgFormat depthFormat msaaSamples VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
  pipeline
    <- auto $ createGraphicsPipeline dev swapExtent
                              [] []
                              shaderStages
                              renderPass
                              pipelineLayout
                              msaaSamples
                              True

  (nextSems, privAttachments) <- auto $ createPrivateAttachments cap swapExtent swapImgFormat msaaSamples
  framebuffers <- mapM
    (auto . createFramebuffer dev renderPass swapExtent . (privAttachments <>) . (:[]))
    swapImgViews

  let renderContext = RenderContext { pipeline, renderPass, pipelineLayout, extent=swapExtent }
  return (framebuffers, nextSems, renderContext)



-- Id of per-material descriptor set
materialSetId :: Word32
materialSetId = 1

pushTransform :: VkCommandBuffer -> VkPipelineLayout -> Mat44f -> Program ()
pushTransform cmdBuf pipelineLayout df =
  liftIO $ thawPinDataFrame df >>= flip withDataFramePtr
    (vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 0 64 . castPtr)

pushPos :: VkCommandBuffer -> VkPipelineLayout -> Vec2f -> Program ()
pushPos cmdBuf pipelineLayout df =
  liftIO $ thawPinDataFrame df >>= flip withDataFramePtr
    (vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 64 8 . castPtr)

pushSize :: VkCommandBuffer -> VkPipelineLayout -> Vec2f -> Program ()
pushSize cmdBuf pipelineLayout df =
  liftIO $ thawPinDataFrame df >>= flip withDataFramePtr
    (vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 72 8 . castPtr)

pushUVPos :: VkCommandBuffer -> VkPipelineLayout -> Vec2f -> Program ()
pushUVPos cmdBuf pipelineLayout df =
  liftIO $ thawPinDataFrame df >>= flip withDataFramePtr
    (vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 80 8 . castPtr)

pushUVSize :: VkCommandBuffer -> VkPipelineLayout -> Vec2f -> Program ()
pushUVSize cmdBuf pipelineLayout df =
  liftIO $ thawPinDataFrame df >>= flip withDataFramePtr
    (vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 88 8 . castPtr)


data DescrBindInfo = DescrBindInfo
  { descrSet       :: VkDescriptorSet
  , dynamicOffsets :: [Word32]
  }

bindDescrSet :: VkCommandBuffer -> VkPipelineLayout -> Word32 -> DescrBindInfo -> Program ()
bindDescrSet cmdBuf pipelineLayout descrSetId DescrBindInfo{..} = locally $ do
  descrSetPtr <- newArrayRes [descrSet]
  let descrSetCnt = 1
  let dynOffCnt = fromIntegral $ length dynamicOffsets
  dynOffPtr <- newArrayRes dynamicOffsets
  liftIO $ vkCmdBindDescriptorSets cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout
    descrSetId descrSetCnt descrSetPtr dynOffCnt dynOffPtr


-- | Stuff that is recreated by myAppNewSwapchain as a side effect
data RenderContext
  = RenderContext
  { pipeline       :: VkPipeline
  , renderPass     :: VkRenderPass
  , pipelineLayout :: VkPipelineLayout
  , extent         :: VkExtent2D
  }


myAppNewWindow :: Chan Event -> GLFW.Window -> Program WindowState
myAppNewWindow eventChan window = do
  let keyCallback _ key _ keyState _ =
        writeChan eventChan $ KeyEvent key keyState
  liftIO $ GLFW.setKeyCallback window (Just keyCallback)
  return WindowState {..}

myAppMainThreadHook :: WindowState -> IO ()
myAppMainThreadHook WindowState {..} = return ()

myAppStart :: MVar ViewModel -> MVar ViewState -> WindowState -> EngineCapability -> Program MyAppState
myAppStart viewModel viewState winState cap@EngineCapability{ dev, queueFam } = do
  shaderStages <- loadShaders cap
  (materialDSL, pipelineLayout) <- makePipelineLayouts dev
  -- TODO beware of automatic resource lifetimes when making assets dynamic
  assets <- loadAssets cap materialDSL
  renderContextVar <- newEmptyMVar
  latestFrameTime <- newEmptyMVar
  secCmdBufsChan <- newChan
  let nBufs = 6 -- TODO determine
  let makeBufSet = VS.replicateM nBufs $ newSecondaryCmdBuf dev queueFam
  writeList2Chan secCmdBufsChan =<< replicateM Graphics.maxFramesInFlight makeBufSet
  return $ MyAppState{..}

myAppNewSwapchain :: MyAppState -> SwapchainInfo -> Program ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)])
myAppNewSwapchain MyAppState{..} swapInfo = do
  _ <- tryTakeMVar renderContextVar
  (framebuffers, nextSems, renderContext) <- prepareRender cap swapInfo shaderStages pipelineLayout
  putMVar renderContextVar renderContext
  oldVs <- takeMVar viewState
  putMVar viewState $ oldVs { aspectRatio = extentToAspect (extent renderContext) }
  return (framebuffers, nextSems)



recordSprite :: VkCommandBuffer -> Program ()
recordSprite cmdBuf =
  liftIO $ vkCmdDraw cmdBuf
    6 1 0 0 -- vertex count, instance count, first vertex, first instance

newSecondaryCmdBuf :: VkDevice -> Word32 -> Program VkCommandBuffer
newSecondaryCmdBuf dev queueFam = do
    cmdPool <- auto $ metaCommandPool dev queueFam
      VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
    let metaSecCmdBufs = metaCommandBuffers dev cmdPool VK_COMMAND_BUFFER_LEVEL_SECONDARY
    head <$> auto (metaSecCmdBufs 1)

makeSecondaryCmdBufBeginInfo :: VkRenderPass -> Word32 -> Maybe VkFramebuffer -> VkCommandBufferBeginInfo
makeSecondaryCmdBufBeginInfo renderPass subpassIndex mayFramebuffer =
  makeCommandBufferBeginInfo
    (VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT .|. VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT)
    (Just $
      case mayFramebuffer of
        -- TODO build a conditional field setter
        Just framebuffer ->
            createVk @VkCommandBufferInheritanceInfo
            $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
            &* set @"pNext" VK_NULL
            &* set @"renderPass" renderPass
            &* set @"subpass" subpassIndex
            &* set @"framebuffer" framebuffer -- optional
        Nothing ->
            createVk @VkCommandBufferInheritanceInfo
            $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
            &* set @"pNext" VK_NULL
            &* set @"renderPass" renderPass
            &* set @"subpass" subpassIndex
    )

myAppRenderFrame :: MyAppState -> RenderFun
myAppRenderFrame appState@MyAppState{ cap, renderContextVar, secCmdBufsChan }
  framebuffer waitSemsWithStages signalSems = do
  retBox <- newEmptyMVar
  _ <- forkProg $ run retBox
  takeMVar retBox

  where
  run retBox = do
    let EngineCapability{ .. } = cap
    managedCmdBuf <- acquireCommandBuffer cmdCap
    let cmdBuf = actualCmdBuf managedCmdBuf
    let cmdbBI = makeCommandBufferBeginInfo VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT Nothing
    withVkPtr cmdbBI $ runVk . vkBeginCommandBuffer cmdBuf

    RenderContext{ renderPass, extent } <- readMVar renderContextVar
    let renderPassBeginInfo = createRenderPassBeginInfo renderPass framebuffer extent
    withVkPtr renderPassBeginInfo $ \rpbiPtr ->
      liftIO $ vkCmdBeginRenderPass cmdBuf rpbiPtr VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS

    secCmdBufs <- readChan secCmdBufsChan

    recordFrame appState secCmdBufs

    liftIO $ VS.unsafeWith secCmdBufs (vkCmdExecuteCommands cmdBuf (fromIntegral $ VS.length secCmdBufs))

    liftIO $ vkCmdEndRenderPass cmdBuf
    runVk $ vkEndCommandBuffer cmdBuf

    queueEvent <- postNotify cmdQueue $ makeSubmitInfo waitSemsWithStages signalSems [cmdBuf]
    -- async return because caller doesn't care about internal cleanup
    putMVar retBox queueEvent
    waitDone queueEvent
    releaseCommandBuffer managedCmdBuf
    writeChan secCmdBufsChan secCmdBufs
    -- continuation ends because of forkProg. Auto things get deallocated.

recordFrame :: MyAppState -> VS.Vector VkCommandBuffer -> Program ()
recordFrame MyAppState{..} cmdBufs = do
  t <- liftIO $ GLFW.getTime >>= \case
    Just time -> return time
    Nothing -> error "GLFW.getTime failed"
  deltaT <- tryTakeMVar latestFrameTime <&> \case
    Just oldT -> realToFrac $ t - oldT
    Nothing -> 0
  putMVar latestFrameTime t

  ViewModel{..} <- readMVar viewModel
  RenderContext{ pipeline, renderPass } <- readMVar renderContextVar

  -- TODO when zoomed out very far, there are texture atlas edge artifacts again.
  let texSize cmdBuf grid = pushUVSize cmdBuf pipelineLayout (0.999 * grid)
      texPos cmdBuf grid p = pushUVPos cmdBuf pipelineLayout ((p + 0.0005) * grid)

  let Assets {..} = assets
      materialBindInfo = DescrBindInfo (materialDescrSets !! 0) []
      tileGrid = recip (Vec2 8 8)
      tilePos cmdBuf (Vec2 x y) = pushPos cmdBuf pipelineLayout (Vec2 (realToFrac x) (realToFrac y))

  oldVs@ViewState { aspectRatio, camPos } <- takeMVar viewState
  let viewSize = Vec2 (aspectRatio*camHeight) camHeight
      newCamPos@(V2 camX camY) = camStep deltaT camPos
  putMVar viewState $ oldVs { camPos = Just newCamPos }
  -- TODO theoretically could premake secCmdbBI for each framebuffer
  let secCmdbBI = makeSecondaryCmdBufBeginInfo renderPass 0 Nothing
  (mapM_ waitProg =<<) . forM (VS.toList cmdBufs) $ \cmdBuf -> asyncProg $ do
    withVkPtr secCmdbBI $ runVk . vkBeginCommandBuffer cmdBuf
    pushTransform cmdBuf pipelineLayout =<< viewProjMatrix (Vec2 camX camY) viewSize
    liftIO $ vkCmdBindPipeline cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
    bindDescrSet cmdBuf pipelineLayout materialSetId materialBindInfo

    pushSize cmdBuf pipelineLayout (vec2 1 1)
    texSize cmdBuf tileGrid

  -- a bit simplistic. when hot loading assets, better filter the objects that depend on them
  events <- takeMVar loadEvents
  notDone <- filterM (fmap not . isDone) events
  let allDone = null notDone
  putMVar loadEvents notDone

  let groups n elems
        | VS.null elems = []
        | otherwise =
          let (grp, rest) = VS.splitAt n elems
          in grp : groups n rest

  let cmdBuf0 = VS.head cmdBufs
  when allDone $ do
    -- walls
    let nThreads = fromIntegral $ VS.length cmdBufs
        nWalls :: Float = fromIntegral $ VS.length walls
        n :: Int = max 1 $ ceiling $ nWalls / nThreads
        wallGroups = groups n walls
    (mapM_ waitProg =<<) . forM (zip (VS.toList cmdBufs) wallGroups) $ \(cmdBuf, walls) ->
      asyncProg $ do
        texPos cmdBuf tileGrid (Vec2 2 4)
        VS.forM_ walls $ \wallPos -> do
          tilePos cmdBuf wallPos
          recordSprite cmdBuf

    -- enemies
    texPos cmdBuf0 tileGrid (Vec2 2 2)

    forM_ enemies $ \pos -> do
      tilePos cmdBuf0 pos
      recordSprite cmdBuf0

    -- player
    texPos cmdBuf0 tileGrid (Vec2 0 0)
    -- horizontal flipping (need to reset texSize after that):
    -- texPos tileGrid (Vec2 4 0)
    -- texSize $ Vec2 (-1) 1 * tileGrid
    tilePos cmdBuf0 playerPos
    recordSprite cmdBuf0

    -- dir
    texPos cmdBuf0 tileGrid (Vec2 5 3)
    tilePos cmdBuf0 (playerPos + dirIndicator)
    recordSprite cmdBuf0

  VS.forM_ cmdBufs $ \cmdBuf -> runVk $ vkEndCommandBuffer cmdBuf

data WindowState
  = WindowState
  { window    :: GLFW.Window
  }

data MyAppState
  = MyAppState
  { shaderStages     :: [VkPipelineShaderStageCreateInfo]
  , pipelineLayout   :: VkPipelineLayout
  , cap              :: EngineCapability
  , secCmdBufsChan   :: Chan (VS.Vector VkCommandBuffer)
  , assets           :: Assets
  , renderContextVar :: MVar RenderContext
  , winState         :: WindowState
  , viewModel        :: MVar ViewModel
  , viewState        :: MVar ViewState
  , latestFrameTime  :: MVar Double
    -- ^ in absolute time based on GLFW.getTime
  }

maxFramesInFlight = 2

runGraphics :: [EngineFlag] -> Chan Event -> MVar ViewModel -> MVar ViewState -> IO ()
runGraphics flags eventChan viewModel viewState = do
  let app = App
        { windowName = "Some Roguelike"
        , windowSize = (800, 600)
        , windowFullscreen = False
        , flags
        , syncMode = VSync
        , maxFramesInFlight = Graphics.maxFramesInFlight
        , appNewWindow = myAppNewWindow eventChan
        , appMainThreadHook = myAppMainThreadHook
        , appStart = myAppStart viewModel viewState
        , appNewSwapchain = myAppNewSwapchain
        , appRenderFrame = myAppRenderFrame
        }
  runVulkanProgram app
