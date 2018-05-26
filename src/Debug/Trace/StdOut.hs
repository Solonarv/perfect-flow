module Debug.Trace.StdOut (trace, traceShow, printId) where

import           System.IO.Unsafe       (unsafePerformIO)

import           Control.Monad.IO.Class

trace :: String -> a -> a
trace s v = unsafePerformIO $ do
  putStrLn s
  return $! v

traceShow :: Show s => s -> a -> a
traceShow s = trace (show s)

printId :: (MonadIO m, Show a) => a -> m a
printId a = liftIO (print a) >> pure a
