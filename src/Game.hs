{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Game
  ( runGame
  ) where

import           Apecs
import           ApecsExts
import           Apecs.Experimental.Reactive
import           Data.Maybe
import           Data.List (union, (\\))
import           Control.Concurrent
import           Control.Lens hiding (Level, set)
import           Control.Monad
import           Control.Monad.Random
import           Graphics.UI.GLFW   (Key, KeyState)
import qualified Graphics.UI.GLFW   as GLFW
import           Linear.V2             (V2 (..), _x, _y)
import qualified Math.Geometry.Grid           as Grid
import           Math.Geometry.Grid.Octagonal (RectOctGrid, rectOctGrid)
import qualified Math.Geometry.GridMap        as GridMap
import           Numeric.Vector
import           Safe                         (headMay)

import Configuration
import qualified Dungeon
import ViewModel


newtype Position = Position Vec2i deriving (Eq, Ord, Show)
instance Component Position where type Storage Position = Reactive (OrdMap Position) (Map Position)

-- | Get list of entities that have the given Position
{-# INLINE whatsAt #-}
whatsAt :: (MonadIO m, Has w m Position) => Vec2i -> SystemT w m [Entity]
whatsAt = withReactive . ordLookup . Position

{-# INLINE cmapAt #-}
cmapAt :: forall w m cx cy. (MonadIO m, Has w m Position, Get w m cx, Set w m cy)
       => Vec2i -> (cx -> cy) -> SystemT w m ()
cmapAt pos f = whatsAt pos >>= flip cmapOver f

{-# INLINE cmapAtM #-}
cmapAtM :: forall w m cx cy. (MonadIO m, Has w m Position, Get w m cx, Set w m cy)
       => Vec2i -> (cx -> SystemT w m cy) -> SystemT w m ()
cmapAtM pos sys = whatsAt pos >>= flip cmapOverM sys

{-# INLINE cfoldAt #-}
cfoldAt :: forall w m c a. (MonadIO m, Has w m Position, Get w m c)
        => Vec2i -> (a -> c -> a) -> a -> SystemT w m a
cfoldAt pos f a0 = whatsAt pos >>= \es -> cfoldOver es f a0


data Wall = Wall deriving Show
instance Component Wall where type Storage Wall = Map Wall

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

data Enemy = Enemy deriving Show
instance Component Enemy where type Storage Enemy = Map Enemy

newtype Time = Time Double deriving (Show, Num)
instance Semigroup Time where (<>) = error "unexpected use of Semigroup Time <>"
instance Monoid Time where mempty = 0
instance Component Time where type Storage Time = Global Time

newtype Grid = Grid RectOctGrid
instance Component Grid where type Storage Grid = Global Grid
instance Monoid Grid where mempty = Grid $ rectOctGrid 0 0
instance Semigroup Grid where (<>) = error "unexpected use of Semigroup Grid <>"

newtype KeysDown = KeysDown [Key]
instance Component KeysDown where type Storage KeysDown = Global KeysDown
deriving instance Monoid KeysDown
deriving instance Semigroup KeysDown

newtype Direction = Direction (V2 Int)
instance Component Direction where type Storage Direction = Global Direction
instance Monoid Direction where mempty = Direction $ V2 0 1
instance Semigroup Direction where (<>) = error "unexpected use of Semigroup Direction <>"

-- what about ''Camera? see Apecs.Gloss

-- creates type World and function initWorld :: IO World
makeWorld "World" [''Position, ''Wall, ''Player, ''Enemy,
                   ''Time, ''Grid, ''KeysDown,
                   ''Direction]

type System' a = System World a

initialize :: System' ()
initialize = do
  let rows = 100
      columns = 100
  level <- liftIO $ Dungeon.execLevelGen columns rows (Dungeon.rndLvl 3 20 0.7)
  let walls = map fst $ filter (isNothing . snd) $ Dungeon.allCells level
  forM_ walls $ \pos ->
    newEntity (Wall, Position pos)
  let (spawnX, spawnY) = head $ GridMap.keys . GridMap.filter isJust $ level
  _ <- newEntity (Player, Position (vec2 spawnX spawnY))
  replicateM_ 100 $ do
    place <- liftIO $ evalRandIO $ Dungeon.findRandomCell level isJust
    void $ newEntity (Enemy, Position (fst place))
  set global $ Grid $ rectOctGrid rows columns

keyToMotion :: Key -> Maybe (V2 Int)
keyToMotion key =
  case key of
    GLFW.Key'Left  -> Just $ V2 (-1) 0
    GLFW.Key'Right -> Just $ V2 1 0
    GLFW.Key'Up    -> Just $ V2 0 (-1)
    GLFW.Key'Down  -> Just $ V2 0 1
    _              -> Nothing

handleEvent :: Event -> System' ()
handleEvent (KeyEvent key keyState)
  | keyState `elem` [GLFW.KeyState'Pressed, GLFW.KeyState'Repeating]
  = do
      KeysDown keysDown <- get global
      let keysDown' = keysDown `union` [key]
      set global $ KeysDown keysDown'
      Direction dir <- get global
      forM_ (keyToMotion key) $ \motion -> do
        let dir' = signum $ dir + motion
            dir''
              -- opposite motion => opposite dir
              | dir' == 0
              = motion
              -- stuck on diagonal => motion turns dir to horiz/vert
              | dir' == dir
              = motion
              | otherwise
              = dir'
        set global $ Direction dir''
  | keyState == GLFW.KeyState'Released
  = do
      KeysDown keysDown <- get global
      let keysDown' = keysDown \\ [key]
      set global $ KeysDown keysDown'
      Direction (V2 x y) <- get global
      when (null keysDown' && keysDown == [GLFW.Key'Space]) $
        step (Vec2 x y)
handleEvent (Tick time) = set global (Time time)

step :: Vec2i -> System' ()
step motion = cmapM $ \(Player, Position pos) -> do
  Grid grid <- get global
  let pos' = pos + motion
      Vec2 x y = pos'
  there <- whatsAt pos'
  return $ if grid `Grid.contains` (x, y) && null there
    then Right $ Position pos'
    else Left ()

runGame :: MVar ViewModel -> MVar ViewState -> Chan Event -> IO ()
runGame vmVar vsVar eventChan = do
  w <- initWorld
  _ <- forkIO $ forever $ do
    threadDelay $ round (1 / realToFrac tickRate * 1e6)
    t <- GLFW.getTime >>= \case
      Just time -> return time
      Nothing -> error "GLFW.getTime failed"
    writeChan eventChan (Tick t)
  runWith w $ do
    initialize
    forever $ do
      event <- liftIO $ readChan eventChan
      handleEvent event
      when (isTick event) $ do
        oldVm <- liftIO $ readMVar vmVar
        vs <- liftIO $ readMVar vsVar
        newVm <- viewModelUpdate oldVm vs
        void $ liftIO $ swapMVar vmVar newVm


viewModelUpdate :: ViewModel -> ViewState -> System' ViewModel
viewModelUpdate ViewModel{ camHeight, camPos, initialized } ViewState{ aspectRatio } = do
  mayPos <- extract $ \(Player, Position pos) -> pos
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
      idxs = [Vec2 x y | y <- [bound top..bound bottom], x <- [bound left..bound right]]
  Grid grid <- get global
  walls <- fmap catMaybes . forM idxs $ \idx -> do
    ents <- whatsAt idx
    isWall <- or <$> forM ents (\ent -> exists ent $ Proxy @Wall)
    return $ if isWall then Just idx else Nothing
  enemies <- extractAll $ \(Enemy, Position pos) -> pos
  Direction (V2 dx dy) <- get global
  return ViewModel
    { camPos = camPos'
    , camHeight
    , playerPos = Vec2 x y
    , dirIndicator = Vec2 dx dy
    , walls
    , enemies
    , initialized = True
    }

camStep :: V2 Float -> V2 Float -> V2 Float
camStep playerPos camPos = camPos''
  where
    baseSpeed = 2.5 -- approach speed in cells per second at distance 1
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
