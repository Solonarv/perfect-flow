module Apecs.Constant (type Constant) where

import           Data.IORef
import           Data.Maybe

import qualified Apecs.Slice as Slice
import           Apecs.Types

-- | A write-once global store. Writing to it more than once will have no effect.
newtype Constant a = Constant { constVal :: IORef (Maybe a) }

instance Store (Constant a) where
  type Stores (Constant a) = a
  type SafeRW (Constant a) = a
  initStore = Constant <$> newIORef Nothing
  explGet (Constant v) _ = fromMaybe (error "Apecs.Constant: get: no value set") <$> readIORef v
  explSet (Constant v) _ a = readIORef v >>= \case
    Nothing -> writeIORef v (Just a)
    Just _ -> pure ()
  explDestroy _ _ = pure ()
  explMembers = mempty
  explGetUnsafe = explGet
  explSetMaybe = explSet

instance GlobalStore (Constant a)
