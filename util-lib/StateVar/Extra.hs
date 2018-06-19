module Data.StateVar.Extra (
  -- * Non-infix variants of operators from "Data.StateVar", with the state var as the last argument.
  set, set'
  ) where

import           Data.StateVar

-- | See @'($=)'@
set :: (HasSetter t a, MonadIO m) => a -> t -> m ()
set = flip ($=)

-- | See @'($=!)'@
set' :: (HasSetter t a, MonadIO m) => a -> t -> m ()
set' = flip ($=!)
