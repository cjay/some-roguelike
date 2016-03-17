module Main where

import Dungeon
import Data.Maybe
import Control.Arrow
import qualified Math.Geometry.GridMap as GridMap
import FRP.Helm
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard

data GameState = GameState { at :: (Int, Int), lvl :: Level }

step :: (Int, Int) -> GameState -> GameState
step (dx, dy) (state @ GameState { .. }) =
    if isNothing (lvl `getCell` at')
    then state
    else state { at = at' }
    where
      at' = (fst at + dx, snd at + dy)

camStep :: GameState -> (Double, Double) -> (Double, Double)
camStep GameState { .. } (cx, cy) = (cx + rate/targetFPS * (fromIntegral (fst at) - cx),
                                     cy + rate/targetFPS * (fromIntegral (snd at) - cy))
    where rate = 2

targetFPS :: Double
targetFPS = 60

cellWidth :: Double
cellWidth = 20

cellHeight :: Double
cellHeight = 12

render :: (Int, Int) -> Time -> GameState -> (Double, Double) -> Element
render (w, h) time GameState { .. } camPos =
  centeredCollage w h $ player : grid
  where
    position x y = move ((fromIntegral x - fst camPos) * cellWidth, (fromIntegral y - snd camPos) * cellHeight)
    player = uncurry position at $ rotate (turns 0.125 * Time.inSeconds time) $ filled red $ square 8
    grid = [position x y $ filled grey $ rect (cellWidth+1) (cellHeight+1)
                | (x, y) <- (GridMap.keys . GridMap.filter isNothing) lvl]

-- autorepeat :: (Int, Int) -> Int -> (Int, Int)
-- autorepeat (dx, dy)

main :: IO ()
main = do
  level <- execLevelGen 100 100 (rndLvl 3 20 0.7)
  let spawn = head $ GridMap.keys . GridMap.filter isJust $ level
      state = GameState { at = spawn, lvl = level }
      stepper = foldp step state Keyboard.arrows
      camStepper = foldp camStep (fromIntegral *** fromIntegral $ spawn)  (sampleOn time stepper)
      config = defaultConfig { windowTitle = "Rogue" }
      dt = Time.fps targetFPS
      time = foldp (+) 0 dt
      -- rt = (/targetFPS) <~ dt
  run config $ render <~ Window.dimensions ~~ time ~~ stepper ~~ camStepper
