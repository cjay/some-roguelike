module Main where

import           Control.Concurrent
import           Graphics
import           Game
import           ViewModel

startGame :: Chan Event -> IO (MVar ViewModel)
startGame events = do
  viewModel <- newMVar initialViewModel
  _ <- forkIO $ runGame viewModel events
  return viewModel

main :: IO ()
main = runGraphics startGame
