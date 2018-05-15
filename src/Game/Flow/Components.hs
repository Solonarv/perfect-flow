{-# language OverloadedStrings #-}
{-# language UndecidableInstances #-}
module Game.Flow.Components where

import Control.Arrow
import Data.Monoid

import Apecs
  
import Data.Aeson.Types
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import Data.Scientific (toRealFloat)
import Data.Text
import Data.Vector (Vector)

import Apecs.EntityIndex

newtype ResAmount = ResAmount { getResAmount :: Double } deriving (Show, Eq)
instance Component ResAmount where type Storage ResAmount = Map ResAmount

data ResBounds = ResBounds { resBoundsMin :: Double, resBoundsMax :: Double } deriving (Show, Eq)
instance Component ResBounds where type Storage ResBounds = Map ResBounds
instance FromJSON ResBounds where
  parseJSON = withObject "bounds: min, max" $ \o -> ResBounds <$> o .: "min" <*> o .: "max"
defaultResBounds :: ResBounds
defaultResBounds = ResBounds 0 100

newtype ResRegen = ResRegen { getResRegen :: Double } deriving (Show, Eq)
instance Component ResRegen where type Storage ResRegen = Map ResRegen
instance FromJSON ResRegen where
  parseJSON = fmap ResRegen . parseJSON

data ResRenderType = ResRenderAsBar | ResRenderAsCooldown deriving (Show, Eq)
instance Component ResRenderType where type Storage ResRenderType = Map ResRenderType

data CastDirection = ChanneledCast | NormalCast deriving (Show, Eq)
instance FromJSON CastDirection where
  parseJSON = withText expected $ \case
    "normal" -> pure NormalCast
    "channeled" -> pure ChanneledCast
    v -> typeMismatch expected (String v)
    where expected = "'normal' or 'channeled'"

data Castable = Castable
  { castTime :: Double
  , castCost :: [(ResourceTarget, AmountSpec)]
  , castType :: CastDirection
  } deriving (Eq, Show)
instance Component Castable where type Storage Castable = Map Castable
instance FromJSON Castable where
  parseJSON = withObject "castable" $ \o -> do
    casttime <- o .:? "time" .!= 0
    ty <- o .:? "type" .!= NormalCast
    costs <- o .:? "cost" .!= mempty
    pure Castable
      { castTime = casttime
      , castCost = first textToResourceTarget <$> HashMap.toList costs
      , castType = ty
      }

data AmountSpec = Min | Fixed Double | Current | Max deriving (Eq, Show)
instance FromJSON AmountSpec where
  parseJSON val = case val of
    Number n -> pure . Fixed $ toRealFloat n
    String txt -> case txt of
      "max" -> pure Max
      "min" -> pure Min
      "all" -> pure Current
    _ -> typeMismatch "number or one of 'max', 'min', 'all'" val

data ResourceTarget = Self | Other Text deriving (Eq, Show)
instance FromJSON ResourceTarget where
  parseJSON = withText "resource" $ pure . textToResourceTarget
textToResourceTarget :: Text -> ResourceTarget
textToResourceTarget txt = if txt == "self" then Self else Other txt

newtype Casting = Casting { castingProgress :: Double } deriving (Show, Eq)
instance Component Casting where type Storage Casting = Unique Casting

newtype Name = Name { getName :: Text } deriving (Show, Eq, Hashable)
instance Component Name where type Storage Name = EntityIndex Name

newtype DamageDealt = DamageDealt { getDamageDealt :: Sum Double } deriving (Show, Monoid)
instance Component DamageDealt where type Storage DamageDealt = Global DamageDealt

newtype Time = Time { getTime :: Sum Double } deriving (Show, Eq, Monoid)
instance Component Time where type Storage Time = Global Time

data Action
  = Damage Double
  deriving (Eq, Show)

instance FromJSON Action where
  parseJSON = withObject "action" $ \o -> Damage <$> o .: "damage"

newtype OnCastCompleted = OnCastCompleted { castCompletedActions :: Vector Action } deriving (Show, Eq)
instance Component OnCastCompleted where type Storage OnCastCompleted = Map OnCastCompleted