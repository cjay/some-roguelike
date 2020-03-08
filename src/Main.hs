module Main where

import           Lib.Engine.Main (EngineFlag(..))
import           Control.Concurrent
import           Graphics
import           Game
import           ViewModel

startGame :: Chan Event -> IO (MVar ViewModel, MVar ViewState)
startGame events = do
  viewModel <- newMVar initialViewModel
  viewState <- newMVar initialViewState
  _ <- forkIO $ runGame viewModel viewState events
  return (viewModel, viewState)

main :: IO ()
main = runGraphics [Validation] startGame
