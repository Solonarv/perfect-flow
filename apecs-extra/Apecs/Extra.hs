module Apecs.Extra where

import           Data.Kind

import           Apecs

type family HasAll (w :: *) (cs :: [*]) :: Constraint where
  HasAll w '[] = ()
  HasAll w (c ': cs) = (Has w c, HasAll w cs)

class MonadIO m => MonadSystem w m where
  liftSystem :: System w a -> m a
  liftSystem act = getWorld >>= runSystem act
  {-# inline liftSystem #-}

  getWorld :: m w
  getWorld = liftSystem (System ask)
  {-# inline getWorld #-}

  {-# MINIMAL liftSystem | getWorld #-}

instance MonadSystem w (System w) where
  liftSystem = id
  {-# inline liftSystem #-}
  getWorld = System ask
  {-# inline getWorld #-}
