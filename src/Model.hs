module Model where

import           Dungeon       (Level)

import           Helm.Keyboard (Key)
import           Helm.Time     (Time)
import           Linear.V2     (V2)

data GameState = GameState
  { playerAt :: V2 Int
  , lvl      :: Level
  }

data Model = Model
  { state    :: GameState
  , camAt    :: V2 Double
  , winSize  :: V2 Int
  , time     :: Time
  , keysDown :: [Key]
  , dir      :: V2 Int
  }
