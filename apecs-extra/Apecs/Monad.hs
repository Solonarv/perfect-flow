{-# LANGUAGE AllowAmbiguousTypes #-}
module Apecs.Monad where

import           Control.Monad.Fix

import           Apecs
import           UnliftIO

-- | UnliftIO - safe because System is just a ReaderT
instance MonadUnliftIO (System w) where
  withRunInIO act = do
    world <- System ask
    liftIO $ act (\m -> runWith world m)

-- | MonadFix - safe because System is just a ReaderT
instance MonadFix (System w) where
  mfix k = System (mfix (unSystem . k))

class MonadUnliftIO m => MonadSystem w m where
  liftSystem :: System w a -> m a
  liftSystem act = getWorld >>= liftIO . runSystem act
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

lcimapM_ :: forall w c m. (Has w c, MonadSystem w m) => ((Entity c, c) -> m ()) -> m ()
lcimapM_ body = do
  run <- askRunInIO
  liftSystem @w $ cimapM_ $ liftIO . run . body

lcmapM_ :: forall w c m. (Has w c, MonadSystem w m) => (c -> m ()) -> m ()
lcmapM_ body = do
  run <- askRunInIO
  liftSystem @w $ cmapM_ $ liftIO . run . body
