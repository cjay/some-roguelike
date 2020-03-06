{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Game
  ( runGame
  ) where

import           Apecs
import           Data.Maybe
import           Control.Concurrent
import           Control.Lens hiding (Level, set)
import           Control.Monad
import           Graphics.UI.GLFW   (Key, KeyState)
import qualified Graphics.UI.GLFW   as GLFW
import           Linear.V2             (V2 (..), _x, _y)
import qualified Math.Geometry.GridMap        as GridMap
import           Numeric.Vector

import Configuration
import qualified Dungeon
import ViewModel


newtype Position = Position Vec2i deriving Show
instance Component Position where type Storage Position = Map Position

-- for now walls are static and live only in Global Level:
-- data Wall = Wall deriving Show
-- instance Component Wall where type Storage Wall = Map Wall

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

newtype Time = Time Double deriving (Show, Num)
instance Semigroup Time where (<>) = error "unexpected use of Semigroup Time <>"
instance Monoid Time where mempty = 0
instance Component Time where type Storage Time = Global Time

newtype Level = Level Dungeon.Level
instance Component Level where type Storage Level = Global Level
instance Monoid Level where mempty = Level $ Dungeon.emptyLvl 1 1
instance Semigroup Level where (<>) = error "unexpected use of Semigroup Level <>"

-- what about ''Camera? see Apecs.Gloss

-- creates type World and function initWorld :: IO World
makeWorld "World" [''Position, ''Player, ''Time, ''Level]

type System' a = System World a

initialize :: System' ()
initialize = do
  level <- liftIO $ Dungeon.execLevelGen 100 100 (Dungeon.rndLvl 3 20 0.7)
  let (spawnX, spawnY) = head $ GridMap.keys . GridMap.filter isJust $ level
  _ <- newEntity (Player, Position (vec2 spawnX spawnY))
  set global $ Level level

handleEvent :: Event -> System' ()
handleEvent (KeyEvent key keyState) =
  let motion =
        case (key, keyState) of
          (GLFW.Key'Left, GLFW.KeyState'Released)  -> vec2 (-1) 0
          (GLFW.Key'Right, GLFW.KeyState'Released) -> vec2 1 0
          (GLFW.Key'Up, GLFW.KeyState'Released)    -> vec2 0 (-1)
          (GLFW.Key'Down, GLFW.KeyState'Released)  -> vec2 0 1
          _                                        -> 0
  in step motion
handleEvent (Tick time) = set global (Time time)

step :: Vec2i -> System' ()
step motion = cmapM $ \(Player, Position pos) -> do
  Level lvl <- get global
  let pos' = pos + motion
  return $ if isJust (lvl `Dungeon.getCell` pos')
    then Right $ Position pos'
    else Left ()

runGame :: MVar ViewModel -> MVar ViewState -> Chan Event -> IO ()
runGame vmVar vsVar eventChan = do
  w <- initWorld
  _ <- forkIO $ forever $ do
    threadDelay 16667
    t <- GLFW.getTime >>= \case
      Just time -> return time
      Nothing -> error "GLFW.getTime failed"
    writeChan eventChan (Tick t)
  runWith w $ do
    initialize
    forever $ do
      event <- liftIO $ readChan eventChan
      handleEvent event
      oldVm <- liftIO $ readMVar vmVar
      vs <- liftIO $ readMVar vsVar
      newVm <- viewModelUpdate oldVm vs
      liftIO $ swapMVar vmVar newVm

viewModelUpdate :: ViewModel -> ViewState -> System' ViewModel
viewModelUpdate ViewModel{ camHeight, camPos, initialized } ViewState{ aspectRatio } = do
  mayPos <- cfold (\_ (Player, Position pos) -> Just pos) Nothing
  let Vec2 x y = case mayPos of
        Just pl -> pl
        Nothing -> error "player entity doesn't exist"
      camPos' = if initialized then camStep playerPos camPos else playerPos
        where playerPos = fromIntegral <$> V2 x y
      V2 camX camY = camPos'
      camWidth = aspectRatio * camHeight
      margin = 1
      left = camX - 0.5 * camWidth - margin
      right = camX + 0.5 * camWidth + margin
      top = camY - 0.5 * camHeight - margin
      bottom = camY + 0.5 * camHeight + margin
      -- rounding away from zero
      bound x =
        let x' = ceiling (abs x)
        in if x < 0 then - x' else x'
      topLeftBound = Vec2 (bound left) (bound top)
      bottomRightBound = Vec2 (bound right) (bound bottom)
  Level lvl <- get global
  let walls = map fst $
        filter (isNothing . snd) $
        Dungeon.allCellsBetween lvl topLeftBound bottomRightBound
  return ViewModel
    { camPos = camPos'
    , camHeight
    , playerPos = Vec2 x y
    , walls
    , initialized = True
    }

camStep :: V2 Float -> V2 Float -> V2 Float
camStep playerPos camPos = camPos''
  where baseSpeed = 2.5 -- approach speed in cells per second at distance 1
        minSpeed = 1.5
        dist :: V2 Float = playerPos - camPos
        vel :: V2 Float = pure baseSpeed * dist -- inherits sign of dist
        vel' = fmap (max minSpeed . abs) vel * signum vel
        step = vel' / pure (fromIntegral tickRate)
        camAt' = camPos + step
        -- now prevent overshoot
        dist' = playerPos - camAt'
        snap _ax = if signum (dist'^._ax) == signum (dist^._ax) then camAt'^._ax else playerPos^._ax
        camPos'' = V2 (snap _x) (snap _y)
