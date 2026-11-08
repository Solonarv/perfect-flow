{-# LANGUAGE DefaultSignatures #-}
module PF.Data.Named where

import GHC.Records (HasField)
import Symbolize (Symbol)

-- | Many game entities have a name.
-- Most of the time the name is a record field @name :: Text@, this
-- is the default implementation (suitable for @deriving anyclass@).
class Named a where
  nameOf :: a -> Symbol
  default nameOf :: HasField "name" a Symbol => a -> Symbol
  nameOf = (.name)