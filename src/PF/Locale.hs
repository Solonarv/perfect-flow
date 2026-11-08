{-# LANGUAGE OverloadedStrings #-}
module PF.Locale where
import GHC.Records
import Symbolize (Symbol)
import Data.Text (Text)
import PF.Data.HierarchicalTable
import qualified Symbolize as Sym
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as Aeson

import Orphans ()

newtype Locale = Locale {mappings :: HTable Symbol Text}

instance HasField "get" Locale ([Symbol] -> Text) where
  getField = lookupLoc

lookupLoc :: Locale -> [Symbol] -> Text
lookupLoc loc key = fromMaybe (Text.intercalate "/" (Sym.unintern <$> key)) $ loc.mappings.lookup key

loadLocale :: FilePath -> IO Locale
loadLocale fp = do
  contents <- BSL.readFile fp
  case Aeson.eitherDecode contents of
    Left err -> error err
    Right ht -> pure (Locale ht)