{-# LANGUAGE BangPatterns, MagicHash #-}
module PF.Data.Key (Key, intern, internString, keyContent, compareKey) where

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
import Data.Function (on)

-- | Interned string with precomputed hash, intended for fast
-- equality comparison and @HashMap@ operations.
-- Access the @'Text'@ content via the @.record@ field, or via
-- @'keyContent'@.
--
-- To obtain a @Key@, use @'intern'@, or its @'IsString'@ instance.
data Key = UnsafeMkKey { _hash :: !Int, _content :: !Text }

instance HasField "content" Key Text where
  getField = (._content)

-- | Get the underlying @'Text'@ of a key.
keyContent :: Key -> Text
keyContent = (._content)

instance Eq Key where
  !k1 == !k2 = isTrue# (reallyUnsafePtrEquality k1 k2)
    || (k1._hash == k2._hash) && (k1._content == k2._content)

-- | The ordering is consistent with the @'Eq'@ instance, but __NOT__ with
-- the @Ord Text@ instance. For standard lexicographic comparison, use @'compareKey'@.
--
-- This instance minimizes string comparisons when using @Ord@-based containers.
instance Ord Key where
  !k1 `compare` !k2
    | k1 == k2 = EQ
    | otherwise = (k1._hash `compare` k2._hash) <> (k1._content `compare` k2._content)

-- | Compare two keys by their actual content (unlike the @'Ord'@ instance).
compareKey :: Key -> Key -> Ordering
compareKey = compare `on` keyContent

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

-- | Get the @'Key'@ for a given @'Text'@. If is called twice
-- with equal arguments, it will return the /same/ @Key@.
intern :: Text -> Key
intern txt = case lookupKey txt of
  Just k -> k
  Nothing -> unsafePerformIO do
    tbl <- takeMVar globalKeyTable
    let !h = hash txt
        !k = UnsafeMkKey h txt
    tbl' <- evaluate $ HashMap.insert txt k tbl
    putMVar globalKeyTable $! tbl'
    pure k

-- | Like @'intern'@, but taking a @'String'@ instead.
internString :: String -> Key
internString = intern . Text.pack

lookupKey :: Text -> Maybe Key
lookupKey txt = unsafeDupablePerformIO do
  tbl <- readMVar globalKeyTable
  evaluate $ HashMap.lookup txt tbl

instance IsString Key where
  fromString = internString