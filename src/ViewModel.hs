module ViewModel where

import           Graphics.UI.GLFW   (Key, KeyState)
import           Numeric.DataFrame
import           Linear.V2             (V2 (..))

-- | This carries everything that flows from game logic to module Graphics
data ViewModel
  = ViewModel
  { camPos :: V2 Float
  , camHeight :: Float
  -- ^ requested height of the view in world coordinates
  , playerPos :: Vec2i
  , dirIndicator :: Vec2i
  , walls :: [Vec2i]
  , initialized :: Bool
  }

initialViewModel :: ViewModel
initialViewModel = ViewModel
  { camPos = 0
  , camHeight = 20
  , playerPos = 0
  , dirIndicator = 0
  , walls = []
  , initialized = False
  }

-- | This carries everything that flows from module Graphics to the game logic
data ViewState
  = ViewState
  { aspectRatio :: Float
    -- ^ width/height of the
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
