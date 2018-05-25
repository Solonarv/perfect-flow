module Data.StateVar.Util
  ( withStateVar
  , ($=)
  ) where

import           UnliftIO

import           Data.StateVar

withStateVar :: (HasGetter t a, HasSetter t a, MonadUnliftIO m) => t -> a -> m r -> m r
withStateVar var val act = do
  oldVal <- get var
  bracket (var $= val)
          (const $ var $= oldVal)
          (const act)
