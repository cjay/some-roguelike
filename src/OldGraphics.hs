module Graphics (view) where

import           Configuration
import           Model

import qualified Helm
import           Helm.Asset            (Image)
import           Helm.Color
import           Helm.Graphics2D
import           Helm.Graphics2D.Text
import qualified Helm.Time             as Time

import           Data.Maybe            (isJust, isNothing)
import           Linear.V2
import qualified Math.Geometry.GridMap as GridMap

cellSize :: Num a => V2 a
cellSize = fmap fromIntegral $ V2 cellWidth cellHeight

view :: Image e -> Model -> Helm.Graphics e
view bird Model{ state = GameState { playerAt, lvl }, camAt, winSize, time, dir } =
  Helm.Graphics2D $
  center
    -- TODO commented out because cairo doesn't do highdpi yet, only sdl
    -- using round to snap to 0.5 increments because of highdpi
    -- (fmap ((/2) . fromIntegral . round . (*2))
    (fmap (fromIntegral . round)
     (fmap fromIntegral winSize * pure 0.5 - camAt * cellSize)) $
  collage $ floor ++ walls ++ [player, cursor]
  where
    position pos = move $ fmap fromIntegral $ pos * cellSize
    positionC pos = move $ fmap fromIntegral $ pos * cellSize - fmap (`div` 2) cellSize
    player = position playerAt $
      rotate (0.125 * Time.inSeconds time * 2 * pi) $ filled (rgb 1 0 0) $ square 15
      -- text $ color (rgb 1 0 0) $ toText "@"
    --walls = [position (V2 x y) $ filled (rgb 0.5 0.5 0.5) $ rect cellSize
    walls = [positionC (V2 x y) $ image cellSize bird
            | (x, y) <- (GridMap.keys . GridMap.filter isNothing) lvl]
    floor = [positionC (V2 x y) $ move cellSize $ rotate pi $ image cellSize bird
            | (x, y) <- (GridMap.keys . GridMap.filter isJust) lvl]
    cursor =
      position (playerAt + dir) $
      outlined defaultLine { lineColor = rgb 0.8 0.8 0.8 } $
      rect $ cellSize + pure 1
