module Game.Engine.Input where


import Control.Monad.Reader.Class
import Data.HashMap.Strict (HashMap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import SDL

data InputAction = ExitGame | CancelCasting | Cast !Text deriving (Eq, Ord, Show)

newtype KeyMap = KeyMap { keyMappings :: HashMap SDL.Keycode KeyModifierActionMap}

newtype KeyModifierActionMap = KeyModifierActionMap { keyModifierActions :: Map VirtualKeyModifiers InputAction}

data VirtualKeyModifiers = VirtualKeyModifiers { vkmodShift :: Bool, vkmodAlt :: Bool, vkmodCtrl :: Bool} deriving (Eq, Ord, Show)

getActionByModifiers
  :: MonadReader KeyModifierActionMap m
  => VirtualKeyModifiers
  -> m (Maybe InputAction)
getActionByModifiers mods = reader $ \(KeyModifierActionMap modmap) ->
  let (lo, exact, _hi) = Map.splitLookup mods modmap
  in  case exact of
        Just act -> Just act
        Nothing  -> snd <$> Map.lookupMax lo
