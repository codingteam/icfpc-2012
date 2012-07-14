
import Control.Concurrent
import Control.Concurrent.STM

import Handler

output :: Int -> IO ()
output n = do
   putStrLn $ "End: " ++ show n

count :: Int -> TVar Int -> IO ()
count n var
  | n == 0 = return ()
  | otherwise =
    do
    putStrLn $ "Count: " ++ show n
    threadDelay $ 500*1000
    count (n-1) var

main = do
  handleSignals 0 (count 10) output

  

