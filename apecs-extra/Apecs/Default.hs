{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
module Apecs.Default where

import           Control.Applicative
import           Data.IORef
import           Data.Maybe

import           Control.Monad.IO.Class
import           Data.Default

import           Apecs.Stores           (Map)
import           Apecs.Types

newtype DefaultWrapper s = DefaultWrapper s

type DefaultMap a = DefaultWrapper (Map a)

instance (SafeRW s ~ Maybe (Stores s), Default (Stores s), Store s) => Store (DefaultWrapper s) where
  type Stores (DefaultWrapper s) = Stores s
  type SafeRW (DefaultWrapper s) = Stores s
  initStore = DefaultWrapper <$> initStore
  explGet (DefaultWrapper s) = fmap (fromMaybe def) . explGet s
  explSet (DefaultWrapper s) = explSet s
  explDestroy (DefaultWrapper s) = explDestroy s
  explMembers (DefaultWrapper s) = explMembers s
  explGetUnsafe = explGet
  explSetMaybe = explSet

data Fallback s = Fallback (IORef (Maybe (Stores s))) s

instance (SafeRW s ~ Maybe (Stores s), Store s) => Store (Fallback s) where
  type Stores (Fallback s) = Stores s
  type SafeRW (Fallback s) = Maybe (Stores s)
  initStore = Fallback <$> newIORef Nothing <*> initStore
  explGet (Fallback d s) e = liftA2 (<|>) (explGet s e) (readIORef d)
  explSet (Fallback _ s) = explSet s
  explDestroy (Fallback _ s) = explDestroy s
  explMembers (Fallback _ s) = explMembers s
  explGetUnsafe = (fmap fromJust .) . explGet
  explSetMaybe (Fallback _ s) = explSetMaybe s
  explReset (Fallback d s) = writeIORef d Nothing >> explReset s

setFallback :: (Has w a, Storage a ~ Fallback s, Store (Fallback s)) => a -> System w ()
setFallback a = do
  Fallback d _ <- getStore
  liftIO $ writeIORef d (Just a)

defaultFallback :: forall a w s. (Has w a, Storage a ~ Fallback s, Default a, Store (Fallback s)) => System w ()
defaultFallback = setFallback (def @a)

getFallback :: (Has w a, Storage a ~ Fallback s, Store (Fallback s)) => System w (Maybe a)
getFallback = do
  Fallback d _ <- getStore
  liftIO $ readIORef d
