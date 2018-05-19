{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Engine.Input where

import           Control.Applicative
import           Data.Semigroup
import           GHC.Exts                     (IsList (..))

import           Control.Monad.Reader.Class
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Text                    (Text)
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vector
import           Data.Yaml

import           SDL.Input.Keyboard           as SDL

import           Game.Engine.Input.Keycodes
import           Game.Engine.Input.SkillIndex

data InputAction = ExitGame | CancelCasting | Cast !SkillIndex deriving (Eq, Ord, Show)

newtype KeyMap = KeyMap { keyMappings :: Map SDL.Keycode KeyModifierActionMap } deriving (Eq, Ord, Show)
instance Semigroup KeyMap where
  KeyMap l <> KeyMap r = KeyMap $ Map.unionWith (<>) l r
instance Monoid KeyMap where
  mappend = (<>)
  mempty = KeyMap mempty

defaultKeyMap :: KeyMap
defaultKeyMap = entryToSingleton (KeyMapEntry KeycodeF4 [Alt] ExitGame) <> entryToSingleton (KeyMapEntry KeycodeSpace [] CancelCasting)

newtype KeyModifierActionMap = KeyModifierActionMap { keyModifierActions :: Map VirtualKeyModifiers InputAction } deriving (Eq, Ord, Show)
instance Semigroup KeyModifierActionMap where
  KeyModifierActionMap l <> KeyModifierActionMap r = KeyModifierActionMap $ Map.unionWith const l r
instance Monoid KeyModifierActionMap where
  mappend = (<>)
  mempty = KeyModifierActionMap mempty

data VirtualKeyModifiers = VirtualKeyModifiers { vkmodShift :: Bool, vkmodAlt :: Bool, vkmodCtrl :: Bool} deriving (Eq, Ord, Show)
noModifiers :: VirtualKeyModifiers
noModifiers = VirtualKeyModifiers False False False

data VirtualModifierKey = Shift | Alt | Ctrl deriving (Eq, Ord, Enum, Bounded, Show)

activeModifierKeys :: VirtualKeyModifiers -> [VirtualModifierKey]
activeModifierKeys mods = [Shift | vkmodShift mods] <> [Alt | vkmodAlt mods] <> [Ctrl | vkmodCtrl mods]

keyModifiersFromList :: Foldable f => f VirtualModifierKey -> VirtualKeyModifiers
keyModifiersFromList v = VirtualKeyModifiers
  { vkmodShift = Shift `elem` v
  , vkmodAlt = Alt `elem` v
  , vkmodCtrl = Ctrl `elem` v
  }

instance IsList VirtualKeyModifiers where
  type Item VirtualKeyModifiers = VirtualModifierKey
  toList = activeModifierKeys
  fromList = keyModifiersFromList

getActionByModifiers
  :: MonadReader KeyModifierActionMap m
  => VirtualKeyModifiers
  -> m (Maybe InputAction)
getActionByModifiers mods = reader $ \(KeyModifierActionMap modmap) ->
  let (lo, exact, _hi) = Map.splitLookup mods modmap
  in  exact <|> snd <$> Map.lookupMax lo

virtualizeKeyModifiers :: SDL.KeyModifier -> VirtualKeyModifiers
virtualizeKeyModifiers mods = VirtualKeyModifiers
  { vkmodShift = keyModifierLeftShift mods || keyModifierRightShift mods
  , vkmodAlt   = keyModifierLeftAlt mods
    || keyModifierRightAlt mods
    || keyModifierAltGr mods
  , vkmodCtrl  = keyModifierLeftCtrl mods
    || keyModifierRightCtrl mods
    || keyModifierAltGr mods
  }

getVirtualModifier :: VirtualModifierKey -> VirtualKeyModifiers -> Bool
getVirtualModifier = \case
  Shift   -> vkmodShift
  Alt     -> vkmodAlt
  Ctrl -> vkmodCtrl

lookupKeyAction :: MonadReader KeyMap m => SDL.Keysym -> m (Maybe InputAction)
lookupKeyAction (Keysym _scancode keycode modifier) =
  reader $ \(KeyMap mappings) -> do
    modmap <- Map.lookup keycode mappings
    getActionByModifiers (virtualizeKeyModifiers modifier) modmap

data KeyMapEntry = KeyMapEntry SDL.Keycode VirtualKeyModifiers InputAction deriving (Eq, Ord, Show)

entryToSingleton :: KeyMapEntry -> KeyMap
entryToSingleton (KeyMapEntry keycode vkmods action) = KeyMap $ Map.singleton keycode (KeyModifierActionMap $ Map.singleton vkmods action)

keyMapEntries :: KeyMap -> Vector KeyMapEntry
keyMapEntries = Map.foldMapWithKey (\keycode -> fmap (uncurry $ KeyMapEntry keycode) . Vector.fromList . Map.assocs . keyModifierActions) . keyMappings

instance FromJSON KeyMapEntry where
  parseJSON = withObject "keymap entry" $ \o -> KeyMapEntry
    <$> (parseKeycode <$> o .: "key")
    <*> o .:? "modifiers" .!= noModifiers
    <*> o .: "action"
instance ToJSON KeyMapEntry where
  toJSON (KeyMapEntry keycode mods action) = object $ ["modifiers" .= mods | mods /= noModifiers] ++ ["key" .= showKeycode keycode, "action" .= action]

instance FromJSON VirtualModifierKey where
  parseJSON = withText "modifier key" $ \case
    "shift" -> pure Shift
    "alt" -> pure Alt
    "ctrl" -> pure Ctrl
    _ -> fail "expected a modifier key (shift, alt, or ctrl)"
instance ToJSON VirtualModifierKey where
  toJSON = String . \case
    Shift -> "shift"
    Alt -> "alt"
    Ctrl -> "ctrl"

instance FromJSON VirtualKeyModifiers where
  parseJSON = withArray "list of modifier keys" $ fmap keyModifiersFromList . traverse parseJSON
instance ToJSON VirtualKeyModifiers where
  toJSON = toJSON . activeModifierKeys

instance FromJSON InputAction where
  parseJSON = withObject "action" $ \o -> o .: "type" >>= \(t::Text) -> case t of
    "exit-game"      -> pure ExitGame
    "cancel-casting" -> pure CancelCasting
    "cast"           -> Cast <$> o .: "skill"
instance ToJSON InputAction where
  toJSON = \case
    ExitGame -> object ["type" .= ("exit-game" :: Text)]
    CancelCasting -> object ["type" .= ("cancel-casting" :: Text)]
    Cast skill -> object ["type" .= ("cast" :: Text), "skill" .= skill]

instance FromJSON KeyMap where
  parseJSON = withArray "keymap entries" $ fmap (foldMap entryToSingleton) . traverse parseJSON
instance ToJSON KeyMap where
  toJSON = toJSON . keyMapEntries
