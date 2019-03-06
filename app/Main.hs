{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Dungeon

-- import           Control.Lens
import           Control.Arrow
import           Data.List             (union, (\\))
import           Data.Maybe            (isJust, isNothing)
import           Linear.V2
import qualified Math.Geometry.GridMap as GridMap

import qualified Helm
import qualified Helm.Cmd              as Cmd
import           Helm.Color            (rgb)
import           Helm.Engine           (Cmd, Engine, GameConfig (GameConfig),
                                        Key)
import qualified Helm.Engine.SDL       as SDL
import           Helm.Graphics2D       (LineStyle (..), center, collage,
                                        defaultLine, filled, move, outlined,
                                        rect, rotate, square)
import qualified Helm.Keyboard         as Keyboard
import qualified Helm.Sub              as Sub
import           Helm.Time             (Time)
import qualified Helm.Time             as Time
import qualified Helm.Window           as Window

tickRate = 60 -- per second
cellWidth = 20 :: Double
cellHeight = 20 :: Double

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

data Action
  = Idle
  | KeyDown Key
  | KeyUp Key
  | Tick Time
  | WinSize (V2 Int)

step :: V2 Int -> GameState -> GameState
step dir (state @ GameState { lvl, playerAt }) =
  let at' = playerAt + dir in
    if isNothing (lvl `getCell` at')
    then state
    else state { playerAt = at' }

camStep :: GameState -> V2 Double -> V2 Double
camStep GameState { playerAt = V2 x y } (V2 cx cy) = V2 (cx + rate/tickRate * (fromIntegral x - cx))
                                                        (cy + rate/tickRate * (fromIntegral y - cy))
    where rate = 2

keyToMotion :: Key -> V2 Int
keyToMotion k = case k of
  Keyboard.LeftKey  -> V2 (-1) 0
  Keyboard.RightKey -> V2 1 0
  Keyboard.UpKey    -> V2 0 (-1)
  Keyboard.DownKey  -> V2 0 1
  _                 -> V2 0 0

view :: Model -> Helm.Graphics e
view Model{ state = GameState { playerAt, lvl }, camAt = V2 cx cy, winSize = V2 wx wy, time, dir } =
  Helm.Graphics2D $
  center (V2 (fromIntegral wx/2) (fromIntegral wy/2)) $
  collage $ grid ++ [player, cursor]
  where
    position (V2 x y) = move $ V2 ((fromIntegral x - cx) * cellWidth)
                             ((fromIntegral y - cy) * cellHeight)
    player = position playerAt $ rotate (0.125 * Time.inSeconds time * 2 * pi) $ filled (rgb 1 0 0) $ square 15
    grid = [position (V2 x y) $ filled (rgb 0.5 0.5 0.5) $ rect $ V2 (cellWidth+1) (cellHeight+1)
           | (x, y) <- (GridMap.keys . GridMap.filter isNothing) lvl]
    cursor = position (playerAt + dir) $ outlined defaultLine { lineColor = rgb 0.8 0.8 0.8 } $ rect $ V2 (cellWidth+1) (cellHeight+1)

update :: Engine e => Model -> Action -> (Model, Cmd e Action)
update m Idle = (m, Cmd.none)
update m@Model{ state, camAt } (Tick time) = (m { camAt = camStep state camAt, time = time }, Cmd.none)
update m (WinSize v) = (m { winSize = v}, Cmd.none)
update m@Model{ keysDown, dir } (KeyDown key) =
  (m { keysDown = keysDown `union` [key]
     , dir = fmap signum $ dir + (keyToMotion key)
     }
  , Cmd.none)
update m@Model{ state, keysDown, dir } (KeyUp key) =
  let keysDown' = keysDown \\ [key]
      state' = if null keysDown' && keysDown == [Keyboard.SpaceKey]
        then step dir state
        else state
  in (m { state = state', keysDown = keysDown' }, Cmd.none)

main :: IO ()
main = do
  level <- execLevelGen 100 100 (rndLvl 3 20 0.7)
  let (spawnX, spawnY) = head $ GridMap.keys . GridMap.filter isJust $ level
      spawn = V2 spawnX spawnY
      initialModel = Model
        { state = GameState{ playerAt = spawn, lvl = level }
        , camAt = fmap fromIntegral spawn
        , time = 0
        , winSize = V2 0 0
        , keysDown = []
        , dir = V2 0 0
        }
      initial = (initialModel, Window.size WinSize)
      subscriptions = Sub.batch
        [ Time.every (Time.second / tickRate) Tick
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Window.resizes WinSize
        ]
      gameConfig = GameConfig initial update subscriptions view
  engine <- SDL.startupWith $ SDL.defaultConfig { SDL.windowTitle = "Rogue" }
  Helm.run engine gameConfig
