module Data.StateVar.Extra (
  -- * Non-infix variants of operators from "Data.StateVar", with the state var as the last argument.
  setV, setV',
  -- * Alias for 'get' which avoids name clashes
  getV
  ) where

import           Control.Monad.IO.Class

import           Data.StateVar

-- | See @'($=)'@
setV :: (HasSetter t a, MonadIO m) => a -> t -> m ()
setV = flip ($=)

-- | See @'($=!)'@
setV' :: (HasSetter t a, MonadIO m) => a -> t -> m ()
setV' = flip ($=!)

-- | See @'get'@
getV :: (HasGetter t a, MonadIO m) => t -> m a
getV = get

