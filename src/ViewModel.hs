module ViewModel where

import qualified Data.Vector.Storable as V
import           Graphics.UI.GLFW   (Key, KeyState)
import           Numeric.DataFrame
import           Linear.V2             (V2 (..))

-- | This carries everything that flows from Game to Graphics
data ViewModel
  = ViewModel
  { camStep :: Float -> Maybe (V2 Float) -> V2 Float
  -- ^ taking delta t in sec and old cam pos
  , camHeight :: Float
  -- ^ requested height of the view in world coordinates
  , playerPos :: Vec2i
  , dirIndicator :: Vec2i
  , walls :: V.Vector Vec2i
  , enemies :: [Vec2i]
  }

initialViewModel :: ViewModel
initialViewModel = ViewModel
  { camStep = const . const 0
  , camHeight = 20
  , playerPos = 0
  , dirIndicator = 0
  , walls = mempty
  , enemies = mempty
  }

-- | This carries everything that flows from Graphics to Game
data ViewState
  = ViewState
  { aspectRatio :: Float
    -- ^ width/height of the window surface
  , camPos :: Maybe (V2 Float)
  }

initialViewState :: ViewState
initialViewState = ViewState
  { aspectRatio = 16/9
  , camPos = Nothing
  }

data Event
  = KeyEvent Key KeyState
  | Begin