{-# LANGUAGE NamedFieldPuns  #-}

module Main where

import           Dungeon

import           Control.Arrow
import           Data.Maybe
import           Linear.V2
import qualified Math.Geometry.GridMap as GridMap

import qualified Helm
import qualified Helm.Cmd              as Cmd
import           Helm.Color            (rgb)
import           Helm.Engine           (Cmd, Engine, GameConfig (GameConfig),
                                        Key)
import qualified Helm.Engine.SDL       as SDL
import           Helm.Graphics2D       (center, collage, filled, move, rect,
                                        rotate, square)
import qualified Helm.Keyboard         as Keyboard
import qualified Helm.Sub              as Sub
import           Helm.Time             (Time)
import qualified Helm.Time             as Time
import qualified Helm.Window           as Window

tickRate = 60 -- per second
cellWidth = 20 :: Double
cellHeight = 20 :: Double

data GameState = GameState
  { at  :: (Int, Int)
  , lvl :: Level
  }

data Model = Model
  { state   :: GameState
  , cam     :: (Double, Double)
  , winSize :: V2 Int
  , time    :: Time
  }

data Action
  = Idle
  | Move (Int, Int)
  | Tick Time
  | WinSize (V2 Int)

step :: (Int, Int) -> GameState -> GameState
step (dx, dy) (state @ GameState { lvl, at = (x, y) }) =
  let at' = (x + dx, y + dy) in
    if isNothing (lvl `getCell` at')
    then state
    else state { at = at' }

camStep :: GameState -> (Double, Double) -> (Double, Double)
camStep GameState { at = (x, y) } (cx, cy) = (cx + rate/tickRate * (fromIntegral x - cx),
                                              cy + rate/tickRate * (fromIntegral y - cy))
    where rate = 2

handleKey :: Key -> Action
handleKey k = case k of
  Keyboard.LeftKey  -> Move (-1, 0)
  Keyboard.RightKey -> Move (1, 0)
  Keyboard.UpKey    -> Move (0, -1)
  Keyboard.DownKey  -> Move (0, 1)
  _                 -> Idle

view :: Model -> Helm.Graphics e
view Model{ state = GameState {at, lvl}, cam = (cx, cy), winSize = V2 wx wy, time } =
  Helm.Graphics2D $
  center (V2 (fromIntegral wx/2) (fromIntegral wy/2)) $
  collage $ grid ++ [player]
  where
    position x y = move $ V2 ((fromIntegral x - cx) * cellWidth)
                             ((fromIntegral y - cy) * cellHeight)
    player = uncurry position at $ rotate (0.125 * Time.inSeconds time * 2 * pi) $ filled (rgb 1 0 0) $ square 15
    grid = [position x y $ filled (rgb 0.5 0.5 0.5) $ rect $ V2 (cellWidth+1) (cellHeight+1)
                | (x, y) <- (GridMap.keys . GridMap.filter isNothing) lvl]


update :: Engine e => Model -> Action -> (Model, Cmd e Action)
update m Idle = (m, Cmd.none)
update m@Model{ state } (Move dv) = (m { state = step dv state }, Cmd.none)
update m@Model{ state, cam } (Tick time) = (m { cam = camStep state cam, time = time }, Cmd.none)
update m (WinSize v) = (m { winSize = v}, Cmd.none)

main :: IO ()
main = do
  level <- execLevelGen 100 100 (rndLvl 3 20 0.7)
  let spawn = head $ GridMap.keys . GridMap.filter isJust $ level
      initial = (Model { state = GameState{ at = spawn, lvl = level }, cam = (fromIntegral *** fromIntegral $ spawn), time = 0, winSize = V2 0 0 },
                 Window.size WinSize)
      subscriptions = Sub.batch [Time.every (Time.second / tickRate) Tick, Keyboard.presses handleKey, Window.resizes WinSize]
      gameConfig = GameConfig initial update subscriptions view
  engine <- SDL.startupWith $ SDL.defaultConfig { SDL.windowTitle = "Rogue" }
  Helm.run engine gameConfig
