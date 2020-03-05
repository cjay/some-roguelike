module ViewModel where

import           Graphics.UI.GLFW   (Key, KeyState)
import           Numeric.DataFrame

-- | This carries everything that flows from game logic to module Graphics
data ViewModel
  = ViewModel
  { camPos :: Vec2f
  , camHeight :: Float
  -- ^ requested height of the view in world coordinates
  , playerPos :: Vec2i
  , walls :: [Vec2i]
  }

initialViewModel :: ViewModel
initialViewModel = ViewModel
  { camPos = 0
  , camHeight = 20
  , playerPos = 0
  , walls = []
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
