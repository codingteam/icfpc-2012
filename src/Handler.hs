
module Handler where

import Control.Concurrent
import Control.Concurrent.STM
import System.Posix.Signals
import System.Exit

import Lifter

type ExitLock = TMVar ExitCode

handleSignals :: a -> (TVar a -> IO b) -> (a -> IO ()) -> IO ()
handleSignals def fn output = do
  var <- atomically $ newTVar def
  mvar <- atomically $ newEmptyTMVar
  installHandler sigINT (Catch $ handler mvar var output) Nothing
  forkIO (fn var >> atomically (putTMVar mvar ExitSuccess))
  exitOnSignal mvar
  result <- atomically $ readTVar var
  output result

exitOnSignal :: ExitLock -> IO ()
exitOnSignal var = atomically (takeTMVar var) >>= exitWith

handler :: ExitLock -> TVar a -> (a -> IO ()) -> IO ()
handler mvar var output = do
  result <- atomically $ readTVar var
  output result
  atomically $ putTMVar mvar ExitSuccess

