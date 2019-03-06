{-# LANGUAGE NamedFieldPuns #-}

module Graphics (view) where

import           Configuration
import           Model

import qualified Helm
import           Helm.Color
import           Helm.Graphics2D
import           Helm.Graphics2D.Text
import qualified Helm.Time             as Time

import           Data.Maybe            (isJust, isNothing)
import           Linear.V2
import qualified Math.Geometry.GridMap as GridMap

view :: Model -> Helm.Graphics e
view Model{ state = GameState { playerAt, lvl }, camAt = V2 cx cy, winSize = V2 wx wy, time, dir } =
  Helm.Graphics2D $
  center (V2 (fromIntegral wx/2) (fromIntegral wy/2)) $
  collage $ grid ++ [player, cursor]
  where
    position (V2 x y) = move $ V2 ((fromIntegral x - cx) * cellWidth)
                                  ((fromIntegral y - cy) * cellHeight)
    player = position playerAt $
      -- rotate (0.125 * Time.inSeconds time * 2 * pi) $ filled (rgb 1 0 0) $ square 15
      text $ color (rgb 1 0 0) $ toText "@"
    grid = [position (V2 x y) $ filled (rgb 0.5 0.5 0.5) $ rect $ V2 (cellWidth+1) (cellHeight+1)
           | (x, y) <- (GridMap.keys . GridMap.filter isNothing) lvl]
    cursor = position (playerAt + dir) $ outlined defaultLine { lineColor = rgb 0.8 0.8 0.8 } $ rect $ V2 (cellWidth+1) (cellHeight+1)
