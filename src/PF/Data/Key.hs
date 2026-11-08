{-# LANGUAGE BangPatterns, MagicHash #-}
module PF.Data.Key (Key, intern, keyContent) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
import Data.Vector (Vector)
import GHC.Exts
import Data.Hashable
import Control.Exception (evaluate)
import qualified Data.Text as Text
import Text.Read
import GHC.Records (HasField (..))

data Key = UnsafeMkKey { _hash :: !Int, _content :: !Text }

instance HasField "content" Key Text where
  getField = (._content)

keyContent :: Key -> Text
keyContent = (._content)

instance Eq Key where
  !k1 == !k2 = isTrue# (reallyUnsafePtrEquality k1 k2)
    || (k1._hash == k2._hash) && (k1._content == k2._content)

instance Hashable Key where
  hashWithSalt salt k = hashWithSalt salt k._content
  hash k = k._hash

instance Show Key where
  showsPrec p k = showsPrec p k._content

instance Read Key where
  readPrec = do txt <- readPrec; pure $! intern txt

globalKeyTable :: MVar (HashMap Text Key)
globalKeyTable = unsafePerformIO (newMVar HashMap.empty)
{-# NOINLINE globalKeyTable #-}

intern :: Text -> Key
intern txt = case lookupKey txt of
  Just k -> k
  Nothing -> unsafePerformIO do
    tbl <- takeMVar globalKeyTable
    let !h = hash txt
        !k = UnsafeMkKey h txt
    tbl' <- evaluate $ HashMap.insert txt k tbl
    putMVar globalKeyTable tbl'
    pure k

lookupKey :: Text -> Maybe Key
lookupKey txt = unsafeDupablePerformIO do
  tbl <- readMVar globalKeyTable
  evaluate $ HashMap.lookup txt tbl

instance IsString Key where
  fromString = intern . Text.pack