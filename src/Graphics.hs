{-# LANGUAGE Strict #-}
module Graphics
  ( runGraphics
  ) where

import           Control.Concurrent       (forkIO)
import           Control.Monad
import qualified Graphics.UI.GLFW         as GLFW
import           Graphics.Vulkan.Core_1_0
import           Linear.V2             (V2 (..), _x, _y)
import           Numeric.DataFrame

import           Lib.Engine.Config
import           Lib.Engine.Main
import           Lib.Engine.Simple2D
import           Lib.MonadIO.Chan
import           Lib.MonadIO.MVar
import           Lib.Program
import           Lib.Resource
import           Lib.Utils                (orthogonalVk, scale)
import           Lib.Vulkan.Descriptor
import           Lib.Vulkan.Device
import           Lib.Vulkan.Drawing
import           Lib.Vulkan.Engine
import           Lib.Vulkan.Image
import           Lib.Vulkan.Pipeline
import           Lib.Vulkan.Presentation
import           Lib.Vulkan.Queue
import           Lib.Vulkan.Shader
-- import           Lib.Vulkan.UniformBufferObject


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
    [ pushConstantRange VK_SHADER_STAGE_VERTEX_BIT 0 64
    ]

  -- (transObjMems, transObjBufs) <- unzip <$> uboCreateBuffers pdev dev transObjSize maxFramesInFlight
  -- descriptorBufferInfos <- mapM (uboBufferInfo transObjSize) transObjBufs

  -- frameDescrSets <- allocateDescriptorSetsForLayout dev descriptorPool maxFramesInFlight frameDSL

  -- forM_ (zip descriptorBufferInfos frameDescrSets) $
    -- \(bufInfo, descrSet) -> updateDescriptorSet dev descrSet 0 [bufInfo] []

  return (materialDSL, pipelineLayout)



loadAssets :: EngineCapability -> VkDescriptorSetLayout -> Program r Assets
loadAssets cap@EngineCapability { dev, descriptorPool } materialDSL = do
  let texturePaths = map ("textures/" ++) ["texture.jpg", "texture2.jpg", "sprite.png"]
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
  let SwapchainInfo { swapExtent, swapImgFormat } = swapInfo
  msaaSamples <- getMaxUsableSampleCount pdev
  depthFormat <- findDepthFormat pdev

  swapImgViews <- auto $
    mapM (\image -> createImageView dev image swapImgFormat VK_IMAGE_ASPECT_COLOR_BIT 1)
         (swapImgs swapInfo)
  renderPass <- auto $ createRenderPass dev swapInfo depthFormat msaaSamples
  graphicsPipeline
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
  let nextSems = [(colorAttSem, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
                 , (depthAttSem, VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT)
                 ]
  framebuffers
    <- auto $ createFramebuffers dev renderPass swapInfo swapImgViews depthAttImgView colorAttImgView

  return (framebuffers, nextSems, RenderContext graphicsPipeline renderPass pipelineLayout swapExtent)



makeWorld :: ViewModel -> Assets -> Program r (Vec2f, [Object])
makeWorld ViewModel {..} Assets {..} = do

  let objs = flip map walls $
        \(Vec2 x y) ->
          Object
          { materialBindInfo = DescrBindInfo (materialDescrSets !! 1) []
          , modelMatrix = scale 1 1 1 %* translate3 (vec3 (realToFrac x) (realToFrac y) 1)
          }
      player = let Vec2 x y = playerPos in
          Object
          { materialBindInfo = DescrBindInfo (materialDescrSets !! 0) []
          , modelMatrix = scale 1 1 1 %* translate3 (vec3 (realToFrac x) (realToFrac y) 1)
          }
      dir = let Vec2 x y = playerPos + dirIndicator in
          Object
          { materialBindInfo = DescrBindInfo (materialDescrSets !! 2) []
          , modelMatrix = scale 1 1 1 %* translate3 (vec3 (realToFrac x) (realToFrac y) 1)
          }

  -- a bit simplistic. when hot loading assets, better filter the objects that depend on them
  events <- takeMVar loadEvents
  notDone <- filterM (fmap not . isDone) events
  let allDone = null notDone
  putMVar loadEvents notDone

  let V2 x y = camPos

  return (Vec2 x y, if allDone then objs <> [player, dir] else [])

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

myAppRecordFrame :: MyAppState -> VkCommandBuffer -> VkFramebuffer -> Program r ()
myAppRecordFrame MyAppState{..} cmdBuf framebuffer = do
  let WindowState{..} = winState

  vm@ViewModel{ camHeight } <- readMVar viewModel
  (camPos, objs) <- makeWorld vm assets

  renderContext <- readMVar renderContextVar
  ViewState { aspectRatio } <- readMVar viewState
  let viewSize = Vec2 (aspectRatio*camHeight) camHeight
  viewProjTransform <- viewProjMatrix camPos viewSize
  recordAll renderContext viewProjTransform objs cmdBuf framebuffer

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


runGraphics :: [Flag] -> (Chan Event -> IO (MVar ViewModel, MVar ViewState)) -> IO ()
runGraphics flags startGame = do
  let app = App
        { windowName = "Some Roguelike"
        , windowSize = (800, 600)
        , flags
        , syncMode = VSync
        , appNewWindow = myAppNewWindow
        , appMainThreadHook = myAppMainThreadHook
        , appStart = myAppStart startGame
        , appNewSwapchain = myAppNewSwapchain
        , appRecordFrame = myAppRecordFrame
        }
  runVulkanProgram app
