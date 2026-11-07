{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
module PF.Simulation.Level where

import PF.Data.Number
import Data.Text (Text)
import Data.Set (Set)
import Data.Map (Map)
import GHC.Records (HasField)
import PF.Data.Named

data Resource = Resource
  { name :: Text
  , min, max, regen, start :: Number
  }
  deriving (Eq, Ord, Show)
  deriving anyclass Named


data CastStyle = Free | Instant | Normal | Channel | CastChannel
  deriving (Eq, Ord, Show)

isChannelStyle :: CastStyle -> Bool
isChannelStyle = \case Channel; CastChannel -> True; _ -> False

data Ability = Ability
  { name :: Text
  , cast_style :: CastStyle
  , cast_time :: Number
  , channel_time, channel_tick :: Number
  , on_cast_start, on_cast_finish, on_channel_tick :: [Effect]
  , cost, cost_tick :: [Cost]
  }
  deriving (Eq, Ord, Show)
  deriving anyclass Named

data Cost = Cost
  { target :: Text
  , amount :: Number
  , phantom :: Bool  -- ^ if true, don't actually subtract the LiveResource, just check if there'd be enough
  }
  deriving (Eq, Ord, Show)

data Effect
  = Effect_Damage Number
  | Effect_GetResource Text Number
  | Effect_ApplyBuff Text
  | Effect_RemoveBuff_Oldest Text
  | Effect_RemoveBuff_Newest Text
  | Effect_RemoveBuff_All Text
  | Effect_Randomly Text Number Effect
  deriving (Eq, Ord, Show)

data ModifierClass = Mod_AddBase | Mod_Increased | Mod_More | Mod_Increased_Invert | Mod_AddFinal
  deriving (Eq, Ord, Show)

data ModifierWhat = Mod_Damage | Mod_Cost Text | Mod_CastSpeed | Mod_Max | Mod_Regen | Mod_Gained Text
  deriving (Eq, Ord, Show)

data Modifier = Modifier
  { name :: Text
  , targets :: Set Text
  , class_ :: ModifierClass
  , what :: ModifierWhat
  , magnitude :: Number
  }
  deriving (Eq, Ord, Show)
  deriving anyclass Named

-- | What to do when the stack limit is already reached, but an effect
-- wants to add another buff of the same name.
data BuffStackingMode
  = Buff_Discard      -- ^ New buff isn't applied
  | Buff_Override     -- ^ Oldest buff is removed
  | Buff_Refresh      -- ^ Older buffs stay, duration reset
  deriving (Eq, Ord, Show)

data Buff = Buff
  { name :: Text
  , stackingMode :: BuffStackingMode
  , maxStacks :: Int
  , duration :: Number
  , on_apply, on_tick, on_expire, on_remove :: [Effect]
  , tick_time :: Number
  , modifiers :: [Modifier]
  }
  deriving (Eq, Ord, Show)
  deriving anyclass Named

data Level = Level
  { resources :: [Resource]
  , abilities :: [Ability]
  , buffs :: Map Text Buff
  }
  deriving (Eq, Ord, Show)

-- | Wrapper for use with @-XDerivingVia@ to derive an object's name
-- from the name of its @prototype@.
newtype Prototyped a = Prototyped a

instance (Named p, HasField "prototype" a p) => Named (Prototyped a) where
  nameOf (Prototyped x) = nameOf x.prototype