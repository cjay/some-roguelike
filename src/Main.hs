{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Configuration
import qualified Dungeon
import qualified Graphics
import           Model

import           Control.Arrow
import           Control.Lens
import           Data.List             (union, (\\))
import           Data.Maybe            (isJust, isNothing)
import           Linear.Metric         (distance)
import           Linear.V2             (V2 (..), _x, _y)
import qualified Math.Geometry.GridMap as GridMap

import           Helm                  (FPSLimit (..), GameConfig (..),
                                        GameLifecycle (..))
import qualified Helm
import qualified Helm.Cmd              as Cmd
import           Helm.Engine           (Cmd, Engine, Key)
import qualified Helm.Engine.SDL       as SDL
import qualified Helm.Keyboard         as Keyboard
import qualified Helm.Sub              as Sub
import           Helm.Time             (Time)
import qualified Helm.Time             as Time
import qualified Helm.Window           as Window

data Action
  = Idle
  | KeyDown Key
  | KeyUp Key
  | Tick Time
  | WinSize (V2 Int)

step :: V2 Int -> GameState -> GameState
step dir (state @ GameState { lvl, playerAt }) =
  let at' = playerAt + dir in
    if isNothing (lvl `Dungeon.getCell` at')
    then state
    else state { playerAt = at' }

camStep :: GameState -> V2 Double -> V2 Double
camStep GameState { playerAt } camAt = camAt''
  where baseSpeed = 2.5 -- approach speed in cells per second at distance 1
        minSpeed = 1.5
        playerAt_ = fmap fromIntegral playerAt
        dist :: V2 Double = playerAt_ - camAt
        vel :: V2 Double = pure baseSpeed * dist -- inherits sign of dist
        vel' = fmap (max minSpeed . abs) vel * signum vel
        step = vel' / pure (fromIntegral tickRate)
        camAt' = camAt + step
        -- now prevent overshoot
        dist' = playerAt_ - camAt'
        snap _ax = if signum (dist'^._ax) == signum (dist^._ax) then camAt'^._ax else playerAt_^._ax
        camAt'' = V2 (snap _x) (snap _y)

keyToMotion :: Key -> V2 Int
keyToMotion k = case k of
  Keyboard.LeftKey  -> V2 (-1) 0
  Keyboard.RightKey -> V2 1 0
  Keyboard.UpKey    -> V2 0 (-1)
  Keyboard.DownKey  -> V2 0 1
  _                 -> V2 0 0

clockwise_ = [V2 1 0, V2 1 1, V2 0 1, V2 (-1) 1, V2 (-1) 0, V2 (-1) (-1), V2 0 (-1), V2 1 (-1)]
clockwise = cycle clockwise_
counterClockwise = cycle $ reverse clockwise_

rotL :: V2 Int -> V2 Int
rotL dir = dropWhile (/= dir) counterClockwise !! 1

rotR :: V2 Int -> V2 Int
rotR dir = dropWhile (/= dir) clockwise !! 1

update :: Engine e => Model -> Action -> (Model, Cmd e Action)
update m Idle = (m, Cmd.none)
update m@Model{ state, camAt } (Tick time) = (m { camAt = camStep state camAt, time = time }, Cmd.none)
update m (WinSize v) = (m { winSize = v }, Cmd.none)
update m@Model{ keysDown, dir } (KeyDown key) =
  (m { keysDown = keysDown `union` [key]
     , dir =
         if firstPersonNav
         then
           case key of
             Keyboard.LeftKey  -> rotL dir
             Keyboard.RightKey -> rotR dir
             _                 -> dir
         else
           let motion = keyToMotion key
           in if motion == V2 0 0
              then dir
              else
                let dir' = fmap signum $ dir + motion
                in if dir' == V2 0 0 || dir' == dir then motion
                    else dir'
     }
  , Cmd.none)
update m@Model{ state, keysDown, dir } (KeyUp key) =
  let keysDown' = keysDown \\ [key]
      state' =
        if firstPersonNav
        then
          if not $ null keysDown'
          then state
          else case keysDown of
            [Keyboard.UpKey]   -> step dir state
            [Keyboard.DownKey] -> step (-dir) state
            _                  -> state
        else
          if null keysDown' && keysDown == [Keyboard.SpaceKey]
          then step dir state
          else state
  in (m { state = state', keysDown = keysDown' }, Cmd.none)

main :: IO ()
main = do
  level <- Dungeon.execLevelGen 100 100 (Dungeon.rndLvl 3 20 0.7)
  let (spawnX, spawnY) = head $ GridMap.keys . GridMap.filter isJust $ level
      spawn = V2 spawnX spawnY
      initialModel = Model
        { state = GameState{ playerAt = spawn, lvl = level }
        , camAt = fmap fromIntegral spawn
        , time = 0
        , winSize = V2 0 0
        , keysDown = []
        , dir = if firstPersonNav then V2 1 0 else V2 0 0
        }
      initial = (initialModel, Window.size WinSize)
      subscriptions = Sub.batch
        [ Time.every (Time.second / fromIntegral tickRate) Tick
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Window.resizes WinSize
        ]
      imgPath = "bird.png"
  engine <- SDL.startupWith $ SDL.defaultConfig { SDL.windowTitle = "Rogue" }
  SDL.withImage engine imgPath $ \image ->
    let gameConfig = GameConfig (Limited 60) 1
        gameLifecycle = GameLifecycle initial update subscriptions (Graphics.view image)
    in Helm.run engine gameConfig gameLifecycle
