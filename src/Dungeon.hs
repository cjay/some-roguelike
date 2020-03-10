module Dungeon
  ( Level
  , emptyLvl
  , execLevelGen
  , rndLvl
  , getCell
  , findRandomCell
  , allCells
  , allCellsBetween
  , printRndLvl
  ) where

import           Control.Monad.Random
import           Control.Monad.Random.Class
import           Control.Monad.State.Strict
import           Data.List
import qualified Math.Geometry.Grid           as Grid
import           Math.Geometry.Grid.Octagonal (RectOctGrid, rectOctGrid)
import qualified Math.Geometry.GridMap        as GridMap
import           Math.Geometry.GridMap.Lazy   (LGridMap, lazyGridMap)
import           Safe                         (headMay)
import           Numeric.Vector

data Object = At | Enemy | Loot deriving (Show, Eq)
type Cell = Maybe [Object]
type Level = LGridMap RectOctGrid Cell
type LevelGen g a = RandT g (State Level) a

data Rect
  = Rect
  { left   :: Int
  , top    :: Int
  , width  :: Int
  , height :: Int
  }

getLeft :: Rect -> Int
getLeft = left

getTop :: Rect -> Int
getTop = top

getCell_ :: Level -> (Int, Int) -> Cell
getCell_ lvl idx =
  if lvl `Grid.contains` idx
  then lvl GridMap.! idx
  else Nothing

getCell :: Level -> Vec2i -> Cell
getCell l (Vec2 x y) = getCell_ l (x, y)

findRandomCell :: MonadRandom m => Level -> (Cell -> Bool) -> m (Vec2i, Cell)
findRandomCell lvl approve = do
  let (w, h) = Grid.size lvl
      go = do
        x <- getRandomR (0, w-1)
        y <- getRandomR (0, h-1)
        let cell = getCell_ lvl (x, y)
        if approve cell
          then return (Vec2 x y, cell)
          else go
  go

allCells :: Level -> [(Vec2i, Cell)]
allCells lvl = flip map (GridMap.toList lvl) $ \((x, y), c) ->
  (Vec2 x y, c)

allCellsBetween :: Level -> Vec2i -> Vec2i -> [(Vec2i, Cell)]
allCellsBetween lvl (Vec2 left top) (Vec2 right bottom) =
  let idxs = [Vec2 x y | y <- [top..bottom], x <- [left..right]]
  in flip map idxs $ \pos@(Vec2 x y) ->
    (pos, join $ GridMap.lookup (x, y) lvl)

emptyLvl :: Int -> Int -> Level
emptyLvl w h = lazyGridMap (rectOctGrid h w) (replicate (w*h) Nothing)

execLevelGen :: Int -> Int -> LevelGen StdGen a -> IO Level
execLevelGen w h gen = do
  stc <- evalRandT gen <$> newStdGen
  return $ execState stc (emptyLvl w h)

center :: Rect -> (Double, Double)
center Rect { .. } =
  (fromIntegral left + fromIntegral width / 2,
    fromIntegral top + fromIntegral height / 2)

anyCell :: MonadRandom m => Rect -> m (Int, Int)
anyCell Rect { .. } = do
  x <- getRandomR (left, left + width - 1)
  y <- getRandomR (top, top + height - 1)
  return (x, y)

dist :: Rect -> Rect -> Double
dist ra rb =
  let (xa, ya) = center ra
      (xb, yb) = center rb
  in sqrt ((xa - xb)**2 + (ya - yb)**2)

lvlTxt :: Level -> String
lvlTxt lvl = unlines rows where
  (w, h) = Grid.size lvl
  rows = map renderLine [0 .. h-1]
  renderLine y = intersperse ' ' $ map (renderChar y) [0 .. w-1]
  renderChar y x =
    case lvl GridMap.! (x, y) of
      Nothing -> '.'
      Just l ->
        if length l < 10
        then last $ show (length l)
        else '*'

eatCells :: [(Int, Int)] -> LevelGen g ()
eatCells cells = do
  lvl <- get
  put $ foldl' (\l cell -> GridMap.insert cell (Just []) l) lvl cells

rndLvl :: RandomGen g => Int -> Int -> Double -> LevelGen g [Rect]
rndLvl minRoomEdge maxRoomEdge_ touchThresh = do
  (w, h) <- Grid.size <$> get
  descend (Rect 1 1 (w-2) (h-2))
    where
      -- something bigger than maxRoomEdge needs to be divisible without violating
      -- minRoomEdge
      maxRoomEdge = max maxRoomEdge_ (2 * minRoomEdge - 1)
      -- implicit bsp tree by recursion
      descend rect@Rect{..} = do
        let needVert = width > maxRoomEdge
            needHoriz = height > maxRoomEdge
        if not needVert && not needHoriz
          then do
            allowTouchChance :: Double <- getRandomR (0, 1)
            let minRoomWidth = max (width `div` 2) minRoomEdge
                minRoomHeight = max (height `div` 2) minRoomEdge
                space = if allowTouchChance <= touchThresh then 0 else 1
            if width - space < minRoomWidth || height - space < minRoomHeight
              then return []
              else do
                roomWidth <- getRandomR (minRoomWidth, width - space)
                roomHeight <- getRandomR (minRoomHeight, height - space)
                widthOffset <- getRandomR (0, width - roomWidth - space)
                heightOffset <- getRandomR (0, height - roomHeight - space)
                let roomTop = top + heightOffset + space
                    roomLeft = left + widthOffset + space
                eatCells [(roomLeft + x, roomTop + y)
                         | x <- [0 .. roomWidth-1], y <- [0 .. roomHeight-1]]
                return [Rect roomLeft roomTop roomWidth roomHeight]
          else do
            doVertical <- getRandom
            if not needHoriz || needVert && doVertical
              then do
                leftWidth <- getRandomR (minRoomEdge, width - minRoomEdge)
                rectsLeft <- descend rect { width = leftWidth }
                rectsRight <- descend rect { left = left + leftWidth, width = width - leftWidth }
                sequence_ $ do
                  rectA <- headMay $ sortOn getLeft rectsRight
                  rectB <- headMay $ sortOn (dist rectA) rectsLeft
                  return $ makeCorridor rectA rectB
                return (rectsLeft ++ rectsRight)
              else do
                topHeight <- getRandomR (minRoomEdge, height - minRoomEdge)
                rectsTop <- descend rect { height = topHeight }
                rectsBottom <- descend rect { top = top + topHeight, height = height - topHeight }
                sequence_ $ do
                  rectA <- headMay $ sortOn getTop rectsBottom
                  rectB <- headMay $ sortOn (dist rectA) rectsTop
                  return $ makeCorridor rectA rectB
                return (rectsTop ++ rectsBottom)

range :: Int -> Int -> [Int]
range a b =
  if a <= b
  then [a .. b]
  else [b .. a]

makeCorridor :: RandomGen g => Rect -> Rect -> LevelGen g ()
makeCorridor a b = do
  (xa, ya) <- anyCell a
  (xb, yb) <- anyCell b
  let horiz y = eatCells [(x, y) | x <- range xa xb]
      vert x = eatCells [(x, y) | y <- range ya yb]
  horizFirst <- getRandom
  if horizFirst
    then do
      horiz ya
      vert xb
    else do
      vert xa
      horiz yb

-- printRndLvl 100 100 3 20 0.7
printRndLvl :: Int -> Int -> Int -> Int -> Double -> IO ()
printRndLvl w h minRoomEdge maxRoomEdge touchThresh = do
  lvl <- execLevelGen w h (rndLvl minRoomEdge maxRoomEdge touchThresh)
  putStrLn $ lvlTxt lvl
