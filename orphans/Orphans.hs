module Orphans () where

import Data.Aeson
import Symbolize (Symbol)
import Symbolize qualified as Sym

instance FromJSON Symbol where
  parseJSON = withText "Symbol" $ \t -> pure $ Sym.intern t

instance FromJSONKey Symbol where
  fromJSONKey = FromJSONKeyText Sym.intern