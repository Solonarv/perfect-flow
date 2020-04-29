module ForeignUtils where

import Foreign

allocaPeek :: Storable a => (Ptr a -> IO ()) -> IO a
allocaPeek f = alloca $ \ptr -> do f ptr; peek ptr