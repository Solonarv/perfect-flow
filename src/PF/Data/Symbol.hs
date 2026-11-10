{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PF.Data.Symbol where
import Symbolize qualified
import Data.String
import Data.Text (Text)
import Data.Hashable (Hashable)
import Data.Aeson
import Symbolize.Textual

newtype Symbol = Symbol Symbolize.Symbol
  deriving newtype (Eq, Ord, Hashable, Read, Show)

instance IsString Symbol where
  fromString = Symbol . fromString

intern :: Textual a => a -> Symbol
intern = Symbol . Symbolize.intern

unintern :: Textual a => Symbol -> a
unintern (Symbol s) = Symbolize.unintern s

instance FromJSON Symbol where
  parseJSON = withText "Symbol" $ \t -> pure $ intern t

instance FromJSONKey Symbol where
  fromJSONKey = FromJSONKeyText intern