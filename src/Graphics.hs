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

cellSize :: Num a => V2 a
cellSize = fmap fromIntegral $ V2 cellWidth cellHeight

view :: Model -> Helm.Graphics e
view Model{ state = GameState { playerAt, lvl }, camAt, winSize, time, dir } =
  Helm.Graphics2D $
  center
    -- TODO commented out because cairo doesn't do highdpi yet, only sdl
    -- using round to snap to 0.5 increments because of highdpi
    -- (fmap ((/2) . fromIntegral . round . (*2))
    (fmap (fromIntegral . round)
     (fmap fromIntegral winSize * pure 0.5 - camAt * cellSize)) $
  collage $ grid ++ [player, cursor]
  where
    position pos = move $ fmap fromIntegral $ pos * cellSize
    player = position playerAt $
      -- rotate (0.125 * Time.inSeconds time * 2 * pi) $ filled (rgb 1 0 0) $ square 15
      text $ color (rgb 1 0 0) $ toText "@"
    grid = [position (V2 x y) $ filled (rgb 0.5 0.5 0.5) $ rect cellSize
           | (x, y) <- (GridMap.keys . GridMap.filter isNothing) lvl]
    cursor =
      position (playerAt + dir) $
      outlined defaultLine { lineColor = rgb 0.8 0.8 0.8 } $
      rect $ cellSize + pure 1
