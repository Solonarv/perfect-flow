module Data.ResourceCache (ResCache, newCache, collectCache, clearCache, getValue, recreateValue) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Functor
import           Data.IORef
import           Data.Maybe
import           Data.Word

import           Data.Hashable
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap

-- | A resource cache indexed by keys of type @k@ and containing resources of type @v@.
data ResCache k v = ResCache
  { rcMaxAge  :: {-# unpack #-} !Word8
  , rcCreate  :: !(k -> IO v)
  , rcDestroy :: !(k -> v -> IO ())
  , rcStorage :: !(IORef (HashMap k (CacheEntry v)))
  }

data CacheEntry v = CacheEntry
  { ceAge   :: {-# unpack #-} !Word8
  , ceValue :: !v
  }

-- | Create a new, empty cache
newCache :: (MonadIO m, Hashable k, Eq k)
         => (k -> IO v)       -- ^ A function to create a new resource given a key.
         -> Word8             -- ^ The maximum age of an entry; how many garbage collections it should survive before being deleted
         -> (k -> v -> IO ()) -- ^ A destructor to be called when an entry is deleted. Use this to free memory, close files etc.
         -> m (ResCache k v)
newCache create maxAge destroy = ResCache maxAge create destroy <$> liftIO (newIORef HashMap.empty)

-- | Perform a garbage collection on the cache: delete all entries whose age exceeds the maximum age, and increment the other entries' age by 1.
collectCache :: (MonadIO m, Hashable k, Eq k) => ResCache k v -> m ()
collectCache (ResCache maxAge _ destroy storage) = liftIO $ do
  readIORef storage >>= HashMap.foldrWithKey (\k (CacheEntry age val) a -> do a; when (age >= maxAge) $ destroy k val) (pure ())
  atomicModifyIORef' storage
    $ ((,()) .) . HashMap.mapMaybe
    $ \entry ->
      if ceAge entry >= maxAge then Nothing else Just (entry { ceAge = ceAge entry + 1 })

-- | Clear the cache, deleting all stored values.
clearCache :: (MonadIO m, Hashable k, Eq k) => ResCache k v -> m ()
clearCache (ResCache _ _ destroy storage) = liftIO $ do
  readIORef storage >>= HashMap.foldrWithKey (\k (CacheEntry age val) a -> a >> destroy k val) (pure ())
  atomicWriteIORef storage HashMap.empty

-- | Look up a value in the cache; if it doesn't exist, create it and store it.
getValue :: (MonadIO m, Hashable k, Eq k) => ResCache k v -> k -> m v
getValue (ResCache _ create _ storage) k = liftIO $ do
  val <- HashMap.lookup k <$> readIORef storage >>= \case
    Just e -> pure (ceValue e)
    Nothing -> create k
  atomicModifyIORef' storage $ (,()) . HashMap.insert k (CacheEntry 0 val)
  pure val

-- | Force recreating a value, overwriting any cache entries.
recreateValue :: (MonadIO m, Hashable k, Eq k) => ResCache k v -> k -> m v
recreateValue (ResCache _ create destroy storage) k = liftIO $ do
  val <- create k
  HashMap.lookup k <$> readIORef storage >>= traverse_ (destroy k . ceValue)
  atomicModifyIORef' storage $ (,()) . HashMap.insert k (CacheEntry 0 val)
  pure val

-- | Peek into the cache, returning a value if it's present or @Nothing@ if it isn't.
-- This function /will/ reset the entry's age to 0.
peekValue :: (MonadIO m, Hashable k, Eq k) => ResCache k v -> k -> m (Maybe v)
peekValue (ResCache _ _ _ storage) k = liftIO $ do
  val <- HashMap.lookup k <$> readIORef storage
  when (isJust val) $ atomicModifyIORef' storage $ (,()) . HashMap.adjust (\e -> e { ceAge = 0 }) k
  pure (ceValue <$> val)

-- | Delete a value from the cache.
deleteValue :: (MonadIO m, Hashable k, Eq k) => ResCache k v -> k -> m ()
deleteValue (ResCache _ _ destroy storage) k = liftIO . void $
  HashMap.lookup k <$> readIORef storage >>= traverse_ (\(CacheEntry _ val) -> do
    destroy k val
    atomicModifyIORef' storage $ (,()) .HashMap.delete k
    )
