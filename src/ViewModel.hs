module ViewModel where

import           Graphics.UI.GLFW   (Key, KeyState)
import           Numeric.DataFrame

-- | This is the glue between game logic and graphics code.
--   It carries only what the graphics part needs.
data ViewModel
  = ViewModel
  { camPos :: Vec2f
  , playerPos :: Vec2i
  , walls :: [Vec2i]
  }

initialViewModel :: ViewModel
initialViewModel = ViewModel
  { camPos = 0
  , playerPos = 0
  , walls = []
  }

data Event
  = KeyEvent Key KeyState
  | Tick Double
