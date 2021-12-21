module ForeignUtils where

import Foreign

allocaPeek :: Storable a => (Ptr a -> IO ()) -> IO a
allocaPeek f = alloca $ \ptr -> do f ptr; peek ptr

allocaArrayPeek :: Storable a => Int -> (Ptr a -> IO ()) -> IO [a]
allocaArrayPeek len f = allocaArray len $ \ptr -> do f ptr; peekArray len ptr

withArrayLen_ :: Storable a => [a] -> (Int -> Ptr c -> IO b) -> IO b 
withArrayLen_ arr cb = withArrayLen arr (\l p -> cb l (castPtr p))