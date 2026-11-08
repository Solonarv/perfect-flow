module PF.Data.HierarchicalTable where

import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as HashMap

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

-- TODO json de/serialization