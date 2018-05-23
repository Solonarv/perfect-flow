module Debug.Trace.StdOut (trace, traceShow) where

import           System.IO.Unsafe (unsafePerformIO)

trace :: String -> a -> a
trace s v = unsafePerformIO $ do
  putStrLn s
  return $! v

traceShow :: Show s => s -> a -> a
traceShow s = trace (show s)
