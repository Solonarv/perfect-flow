{-# LANGUAGE OverloadedStrings #-}
module PF.Data.HierarchicalTable where

import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as HashMap
import GHC.Records
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Control.Applicative hiding (empty)

data HTable k v = HTable
  { here :: !(Maybe v)
  , children :: HashMap k (HTable k v)
  }
  deriving (Eq, Show)

empty :: HTable k v
empty = HTable Nothing HashMap.empty

lookup :: Hashable k => [k] -> HTable k v -> Maybe v
lookup [] tbl = tbl.here
lookup (k:ks) tbl = HashMap.lookup k tbl.children >>= lookup ks

subtable :: Hashable k => [k] -> HTable k v -> HTable k v
subtable [] tbl = tbl
subtable (k:ks) tbl = maybe empty (subtable ks) $ HashMap.lookup k tbl.children

singleton :: v -> HTable k v
singleton v = HTable (Just v) HashMap.empty

nest :: Hashable k => [k] -> HTable k v -> HTable k v
nest [] t = t
nest (k:ks) t = empty{children = HashMap.singleton k (nest ks t)}

overrideAt :: Hashable k => [k] -> HTable k v -> HTable k v -> HTable k v
overrideAt [] vs tbl = union vs tbl
overrideAt (k:ks) vs tbl = tbl{children = HashMap.alter go k tbl.children}
  where
    go = \case
      Nothing -> Just (nest ks tbl)
      Just old -> Just (overrideAt ks vs old)

union :: Hashable k => HTable k v -> HTable k v -> HTable k v
union = unionWith const

unionWith :: Hashable k => (v -> v -> v) -> HTable k v -> HTable k v -> HTable k v
unionWith f new tbl = HTable
  { here = liftA2 f new.here tbl.here <|> new.here <|> tbl.here
  , children = HashMap.unionWith (unionWith f) new.children tbl.children
  }

instance Hashable k => HasField "lookup"  (HTable k v) ([k] -> Maybe v) where
  getField = flip lookup

instance Hashable k => HasField "subtable" (HTable k v) ([k] -> HTable k v) where
  getField = flip subtable

instance (FromJSONKey k, Hashable k, FromJSON v) => FromJSON (HTable k v) where
  parseJSON = liftA2 (<|>)
    (withObject "HTable" $ \v -> do
      here <- v .:? "_here"
      children <- parseJSON $ Object (KeyMap.delete "_here" v)
      pure $ HTable{..}
    )
    (fmap singleton . parseJSON)