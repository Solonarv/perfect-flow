{-# LANGUAGE OverloadedStrings #-}
module Game.Engine.Settings where

import           Data.Semigroup

import           Data.Aeson
import           Data.Yaml.Include (decodeFileEither)

import           Game.Engine.Input

data GameSettings = GameSettings
  { gameSettingsInterruptOnCast :: Bool
  , gameSettingsKeyMap          :: KeyMap
  } deriving (Eq, Ord, Show)

instance Semigroup GameSettings where
  GameSettings interl keymapl <> GameSettings _interr keymapr = GameSettings interl (keymapl <> keymapr)
instance Monoid GameSettings where
  mappend = (<>)
  mempty = GameSettings True mempty

instance FromJSON GameSettings where
  parseJSON = withObject "settings" $ \o -> GameSettings
    <$> o .:? "interrupt-on-cast" .!= False
    <*> o .: "keymap"
instance ToJSON GameSettings where
  toJSON settings = object
    [ "interrupt-on-cast" .= gameSettingsInterruptOnCast settings
    , "keymap" .= gameSettingsKeyMap settings
    ]

defaultGameSettingsPath :: FilePath
defaultGameSettingsPath = "data/settings.yaml"

loadGameSettings :: FilePath -> IO GameSettings
loadGameSettings fp = decodeFileEither fp >>= either (\e -> mempty <$ print e) pure

class Monad m => MonadSettings m where
  getSettings :: m GameSettings
