module ViewModel where

import           Graphics.UI.GLFW   (Key, KeyState)
import           Numeric.DataFrame
import           Linear.V2             (V2 (..))

-- | This carries everything that flows from Game to Graphics
data ViewModel
  = ViewModel
  { camPos :: V2 Float
  , camHeight :: Float
  -- ^ requested height of the view in world coordinates
  , playerPos :: Vec2i
  , dirIndicator :: Vec2i
  , walls :: [Vec2i]
  , enemies :: [Vec2i]
  , initialized :: Bool
  }

initialViewModel :: ViewModel
initialViewModel = ViewModel
  { camPos = 0
  , camHeight = 20
  , playerPos = 0
  , dirIndicator = 0
  , walls = []
  , enemies = []
  , initialized = False
  }

-- | This carries everything that flows from Graphics to Game
data ViewState
  = ViewState
  { aspectRatio :: Float
    -- ^ width/height of the window surface
  }

initialViewState :: ViewState
initialViewState = ViewState
  { aspectRatio = 16/9
  }

data Event
  = KeyEvent Key KeyState
  | Tick Double

isTick :: Event -> Bool
isTick (Tick _) = True
isTick _ = False
