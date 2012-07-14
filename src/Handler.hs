
module Handler where

import Control.Concurrent.STM
import System.Posix.Signals
import System.Exit

import Lifter

handleSignals :: a -> (TVar a -> IO b) -> (a -> IO ()) -> IO ()
handleSignals def fn output = do
  var <- atomically $ newTVar def
  installHandler sigINT (Catch $ handler var output) Nothing
  fn var
  result <- atomically $ readTVar var
  output result

handler :: TVar a -> (a -> IO ()) -> IO ()
handler var output = do
  result <- atomically $ readTVar var
  output result
  exitWith ExitSuccess
