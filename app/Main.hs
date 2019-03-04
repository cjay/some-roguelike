{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Main where

import Dungeon (Level, execLevelGen, rndLvl, getCell)

import Data.Maybe
import Control.Arrow
import Linear.V2
import qualified Math.Geometry.GridMap as GridMap

import Helm
import Helm.Sub
import Helm.Color
import Helm.Engine
import Helm.Time (Time)
import Helm.Graphics2D
import Helm.Engine.SDL (SDLEngine)
import qualified Helm.Cmd as Cmd
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Time as Time
import qualified Helm.Window as Window
import qualified Helm.Keyboard as Keyboard

data GameState = GameState { at :: (Int, Int), lvl :: Level }
data Model = Model { state :: GameState, cam :: (Double, Double), winSize :: V2 Int, time :: Time }

step :: (Int, Int) -> GameState -> GameState
step (dx, dy) (state @ GameState { .. }) =
    if isNothing (lvl `getCell` at')
    then state
    else state { at = at' }
    where
      at' = (fst at + dx, snd at + dy)

camStep :: GameState -> (Double, Double) -> (Double, Double)
camStep GameState { .. } (cx, cy) = (cx + rate/tickRate * (fromIntegral (fst at) - cx),
                                     cy + rate/tickRate * (fromIntegral (snd at) - cy))
    where rate = 2

tickRate :: Num a => a
tickRate = 60 -- per second

cellWidth :: Double
cellWidth = 20

cellHeight :: Double
cellHeight = 20

renderG :: V2 Double -> Time -> GameState -> (Double, Double) -> Collage e
renderG dims time GameState { .. } camPos =
  center dims $ collage $ player : grid
  where
    position x y = move $ V2 ((fromIntegral x - fst camPos) * cellWidth)
                             ((fromIntegral y - snd camPos) * cellHeight)
    player = uncurry position at $ rotate (0.125 * Time.inSeconds time * 2 * pi) $ filled (rgb 1 0 0) $ square 8
    grid = [position x y $ filled (rgb 0.5 0.5 0.5) $ rect $ V2 (cellWidth+1) (cellHeight+1)
                | (x, y) <- (GridMap.keys . GridMap.filter isNothing) lvl]

-- autorepeat :: (Int, Int) -> Int -> (Int, Int)
-- autorepeat (dx, dy)

data Action = Stay | Move (Int, Int) | Tick Time | WinSize (V2 Int)

handleKey :: Key -> Action
handleKey k = case k of
  Keyboard.LeftKey -> Move (-1, 0)
  Keyboard.RightKey -> Move (1, 0)
  Keyboard.UpKey -> Move (0, -1)
  Keyboard.DownKey -> Move (0, 1)
  _ -> Stay

handleTick :: Time -> Action
handleTick time = Tick time

handleWinSize :: V2 Int -> Action
handleWinSize v = WinSize v

view :: Model -> Graphics e
view Model{ state, cam, winSize = V2 wx wy, time } = Graphics2D $ renderG (V2 (fromIntegral wx/2) (fromIntegral wy/2)) time state cam

update :: Engine e => Model -> Action -> (Model, Cmd e Action)
update m Stay = (m, Cmd.none)
update m@Model{ state } (Move dv) = (m { state = step dv state}, Cmd.none)
update m@Model{ state, cam } (Tick time) = (m { cam = camStep state cam, time = time }, Cmd.none)
update m (WinSize v) = (m { winSize = v}, Cmd.none)

main :: IO ()
main = do
  level <- execLevelGen 100 100 (rndLvl 3 20 0.7)
  let spawn = head $ GridMap.keys . GridMap.filter isJust $ level
      initial = (Model { state = GameState{ at = spawn, lvl = level }, cam = (fromIntegral *** fromIntegral $ spawn), time = 0, winSize = V2 0 0 },
                 Window.size handleWinSize)
      subscriptions = batch [Time.every (Time.second / tickRate) handleTick, Keyboard.presses handleKey, Window.resizes handleWinSize]
      gameConfig = GameConfig initial update subscriptions view
  engine <- SDL.startupWith $ SDL.defaultConfig { SDL.windowTitle = "Rogue" }
  return ()
  run engine gameConfig
