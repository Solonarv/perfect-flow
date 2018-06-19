module Data.ResourceCache
  ( ResCache
  , newCache
  , collectCache
  , clearCache
  , getValue
  , recreateValue
  , module Data.Label
  , MonadResCache(..)
  , gcollectCache
  , gclearCache
  , ggetValue
  , grecreateValue
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Functor
import           Data.IORef
import           Data.Maybe
import           Data.Word
import           GHC.OverloadedLabels
import           GHC.TypeLits

import           Control.Monad.Reader
import           Data.Hashable
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap

import           Data.Label

-- | A resource cache indexed by keys of type @k@ and containing resources of type @v@. An environment of type @e@ is available.
data ResCache e k v = ResCache
  { rcEnv     :: !e
  , rcMaxAge  :: {-# unpack #-} !Word8
  , rcCreate  :: !(e -> k -> IO v)
  , rcDestroy :: !(e -> k -> v -> IO ())
  , rcStorage :: !(IORef (HashMap k (CacheEntry v)))
  }

data CacheEntry v = CacheEntry
  { ceAge   :: {-# unpack #-} !Word8
  , ceValue :: !v
  }

-- | Create a new, empty cache
newCache :: (MonadIO m, Hashable k, Eq k)
         => (e -> k -> IO v)       -- ^ A function to create a new resource given a key.
         -> Word8                  -- ^ The maximum age of an entry; how many garbage collections it should survive before being deleted
         -> (e -> k -> v -> IO ()) -- ^ A destructor to be called when an entry is deleted. Use this to free memory, close files etc.
         -> e                      -- ^ The environment to be passed to create/destroy
         -> m (ResCache e k v)
newCache create maxAge destroy env = ResCache env maxAge create destroy <$> liftIO (newIORef HashMap.empty)

-- | Create a new, empty cache with no environment
newCache_ :: (MonadIO m, Hashable k, Eq k)
          => (k -> IO v)       -- ^ A function to create a new resource given a key.
          -> Word8                  -- ^ The maximum age of an entry; how many garbage collections it should survive before being deleted
          -> (k -> v -> IO ()) -- ^ A destructor to be called when an entry is deleted. Use this to free memory, close files etc.                    -- ^ The environment to be passed to create/destroy
          -> m (ResCache () k v)
newCache_ create maxAge destroy = newCache create maxAge destroy ()

-- | Perform a garbage collection on the cache: delete all entries whose age exceeds the maximum age, and increment the other entries' age by 1.
collectCache :: (MonadIO m, Hashable k, Eq k) => ResCache e k v -> m ()
collectCache (ResCache env maxAge _ destroy storage) = liftIO $ do
  readIORef storage >>= HashMap.foldrWithKey (\k (CacheEntry age val) a -> do a; when (age >= maxAge) $ destroy env k val) (pure ())
  atomicModifyIORef' storage
    $ ((,()) .) . HashMap.mapMaybe
    $ \entry ->
      if ceAge entry >= maxAge then Nothing else Just (entry { ceAge = ceAge entry + 1 })

-- | Clear the cache, deleting all stored values.
clearCache :: (MonadIO m, Hashable k, Eq k) => ResCache e k v -> m ()
clearCache (ResCache env _ _ destroy storage) = liftIO $ do
  readIORef storage >>= HashMap.foldrWithKey (\k (CacheEntry age val) a -> a >> destroy env k val) (pure ())
  atomicWriteIORef storage HashMap.empty

-- | Look up a value in the cache; if it doesn't exist, create it and store it.
getValue :: (MonadIO m, Hashable k, Eq k) => ResCache e k v -> k -> m v
getValue (ResCache env _ create _ storage) k = liftIO $ do
  val <- HashMap.lookup k <$> readIORef storage >>= \case
    Just e -> pure (ceValue e)
    Nothing -> create env k
  atomicModifyIORef' storage $ (,()) . HashMap.insert k (CacheEntry 0 val)
  pure val

-- | Force recreating a value, overwriting any cache entries.
recreateValue :: (MonadIO m, Hashable k, Eq k) => ResCache k v -> k -> m v
recreateValue (ResCache env _ create destroy storage) k = liftIO $ do
  val <- create env k
  HashMap.lookup k <$> readIORef storage >>= traverse_ (destroy env k . ceValue)
  atomicModifyIORef' storage $ (,()) . HashMap.insert k (CacheEntry 0 val)
  pure val

-- | Peek into the cache, returning a value if it's present or @Nothing@ if it isn't.
-- This function /will/ reset the entry's age to 0.
peekValue :: (MonadIO m, Hashable k, Eq k) => ResCache k v -> k -> m (Maybe v)
peekValue (ResCache _ _ _ _ storage) k = liftIO $ do
  val <- HashMap.lookup k <$> readIORef storage
  when (isJust val) $ atomicModifyIORef' storage $ (,()) . HashMap.adjust (\e -> e { ceAge = 0 }) k
  pure (ceValue <$> val)

-- | Delete a value from the cache.
deleteValue :: (MonadIO m, Hashable k, Eq k) => ResCache k v -> k -> m ()
deleteValue (ResCache env _ _ destroy storage) k = liftIO . void $
  HashMap.lookup k <$> readIORef storage >>= traverse_ (\(CacheEntry _ val) -> do
    destroy env k val
    atomicModifyIORef' storage $ (,()) .HashMap.delete k
    )

-- | This monad provides generic versions of the cache operations. Since there may be multiple caches
-- in a given monad, a @'Symbol'@ is used to select the cache to operate on.
class (MonadIO m, Hashable k, Eq k) => MonadResCache k v m where
  getCache :: m (ResCache k v)

-- | @'collectCache'@ using the cache from the monadic context.
gcollectCache :: MonadResCache k v m => Label sym -> m ()
gcollectCache l = getCache l >>= collectCache

-- | @'clearCache'@ using the cache from the monadic context.
gclearCache :: MonadResCache k v m => Label sym -> m ()
gclearCache l = getCache l >>= clearCache

-- | @'getValue'@ using the cache from the monadic context.
ggetValue :: MonadResCache k v m => Label sym -> k -> m v
ggetValue l k = getCache l >>= flip getValue k

-- | @'recreateValue'@ using the cache from the monadic context.
grecreateValue :: MonadResCache k v m => Label sym -> k -> m v
grecreateValue l k = getCache l >>= flip recreateValue k

-- | @'peekValue'@ using the cache from the monadic context.
gpeekValue :: MonadResCache k v m => Label sym -> k -> m (Maybe v)
gpeekValue l k = getCache l >>= flip peekValue k

-- | @'deleteValue'@ using the cache from the monadic context.
gdeleteValue :: MonadResCache k v m => Label sym -> k -> m ()
gdeleteValue l k = getCache l >>= flip deleteValue k
