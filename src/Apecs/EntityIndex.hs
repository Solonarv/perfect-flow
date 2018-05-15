module Apecs.EntityIndex
  ( type EntityIndex
  , lookupEntity
  , lookupEntityUnsafe
  ) where

import Data.Foldable
import Data.IORef
import Data.Maybe

import Apecs.Types

import Control.Monad.IO.Class
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as VU

data EntityIndex k = EntityIndex (IORef (HashMap k Int)) (IORef (IntMap k))
instance (Eq k, Hashable k) => Store (EntityIndex k) where
  type Stores (EntityIndex k) = k
  initStore = EntityIndex <$> newIORef mempty <*> newIORef mempty
  explDestroy (EntityIndex ixes names) ety = do
    oldName <- IntMap.lookup ety <$> readIORef names
    for_ oldName $ modifyIORef' ixes . HashMap.delete
    modifyIORef' names $ IntMap.delete ety
  {-# inline explDestroy #-}
  explMembers (EntityIndex _ names) = VU.fromList . IntMap.keys <$> readIORef names
  {-# inline explMembers #-}
  explExists (EntityIndex _ names) ety = IntMap.member ety <$> readIORef names
  {-# inline explExists #-}
  explReset (EntityIndex ixes names) = writeIORef ixes mempty >> writeIORef names mempty
  type SafeRW (EntityIndex k) = Maybe k
  explGetUnsafe (EntityIndex _ names) ety = (IntMap.! ety) <$> readIORef names
  {-# inline explGetUnsafe #-}
  explGet (EntityIndex _ names) ety = IntMap.lookup ety <$> readIORef names
  {-# inline explGet #-}
  explSet (EntityIndex ixes names) ety name = do
    oldName <- IntMap.lookup ety <$> readIORef names
    oldIndex <- HashMap.lookup name <$> readIORef ixes
    for_ oldName  $ modifyIORef' ixes  . HashMap.delete
    for_ oldIndex $ modifyIORef' names . IntMap.delete
    modifyIORef' ixes $ HashMap.insert name ety
    modifyIORef' names $ IntMap.insert ety name
  {-# inline explSet #-}
  explSetMaybe index ety = \case
    Nothing -> explDestroy index ety
    Just name -> explSet index ety name
  {-# inline explSetMaybe #-}
  explCmapM_ (EntityIndex _ names) act = liftIO (readIORef names) >>= mapM_ act
  {-# inline explCmapM_ #-}
  explCimapM_ (EntityIndex _ names) act = liftIO (readIORef names) >>= mapM_ act . IntMap.assocs
  {-# inline explCimapM_ #-}

lookupEntity
  :: (Has w k, Storage k ~ EntityIndex k, Eq k, Hashable k)
  => k
  -> System w (Maybe (Entity k))
lookupEntity k = do
  EntityIndex ixes _ <- getStore
  liftIO $ fmap Entity . HashMap.lookup k <$> readIORef ixes

lookupEntityUnsafe
  :: (Has w k, Storage k ~ EntityIndex k, Eq k, Hashable k)
  => k
  -> System w (Entity k)
lookupEntityUnsafe = fmap fromJust . lookupEntity
