{-# LANGUAGE DefaultSignatures #-}
module PF.Data.Named where

import Data.Text (Text)
import GHC.Records (HasField)

-- | Many game entities have a name.
-- Most of the time the name is a record field @name :: Text@, this
-- is the default implementation (suitable for @deriving anyclass@).
class Named a where
  nameOf :: a -> Text
  default nameOf :: HasField "name" a Text => a -> Text
  nameOf = (.name)