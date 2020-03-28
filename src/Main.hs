module Main where

import           Vulkyrie.Engine.Main (EngineFlag(..))
import           Control.Concurrent
import           Control.Monad                  ( void )
import           Graphics
import           Game
import           ViewModel

-- TODO the chan should be a bounded chan. millions of accumulated input events is an error condition.
main :: IO ()
main = do
  viewModel <- newMVar initialViewModel
  viewState <- newMVar initialViewState
  eventChan <- newChan
  void . forkIO $ runGame eventChan viewModel viewState
  runGraphics [] eventChan viewModel viewState
