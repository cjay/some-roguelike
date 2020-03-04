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
import           Control.Monad
import           Graphics.UI.GLFW   (Key, KeyState)
import qualified Graphics.UI.GLFW   as GLFW
import           Numeric.Vector
import qualified Math.Geometry.GridMap        as GridMap

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
  in cmap $ \(Player, Position pos) -> Position (pos + motion)
handleEvent (Tick time) = set global (Time time)


runGame :: MVar ViewModel -> Chan Event -> IO ()
runGame gsVar eventChan = do
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
      -- now the consequences of the event can propagate
      -- modifyMVar_ gsVar viewModelUpdate -- that would need unliftIO
      oldGs <- liftIO $ takeMVar gsVar
      newGs <- viewModelUpdate oldGs
      liftIO $ putMVar gsVar newGs

-- TODO filter out tiles outside of camera
viewModelUpdate :: ViewModel -> System' ViewModel
viewModelUpdate oldGs = do
  mPos <- cfold (\_ (Player, Position pos) -> Just pos) Nothing
  let Vec2 x y = case mPos of
        Just pl -> pl
        Nothing -> error "player entity doesn't exist"
  -- walls <- cfold (\ws (Wall, Position w) -> w:ws) []
  Level lvl <- get global
  let walls = map fst $ filter (isNothing . snd) $ Dungeon.allCells lvl
  return oldGs { camPos = Vec2 (fromIntegral x) (fromIntegral y), walls}
