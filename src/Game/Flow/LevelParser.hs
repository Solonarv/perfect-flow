{-# language OverloadedStrings #-}
{-# language OverloadedLists #-}
module Game.Flow.LevelParser where

import Control.Applicative
import Control.Exception (throwIO)
import Data.Foldable
import Data.Maybe
import Data.Void

import Apecs

import Control.Monad.IO.Class
import Data.Aeson
import Data.Vector (Vector)
import Data.Yaml.Include

import Apecs.Util
import Game.Engine.Input.SkillIndex
import Game.Flow.Components

data EntityDescription = EntityDescription
  { etyName :: Name 
  , etyResourceSpec :: Maybe ResourceSpecification
  , etyCastable :: Maybe Castable
  , etyCastCompletion :: Maybe (Vector Action)
  , etySkillIndex :: Maybe SkillIndex
  } deriving (Eq, Show)

data ResourceSpecification = ResourceSpecification
  { resSpecBounds :: ResBounds
  , resSpecRegen :: Maybe ResRegen
  , resSpecStartValue :: AmountSpec
  } deriving (Eq, Show)

instance FromJSON EntityDescription where
  parseJSON = withObject "entity" $ \o -> EntityDescription
      <$> (Name <$> o .: "name")
      <*> resourceSpecificationP o
      <*> o .:? "castable"
      <*> o .:? "cast-completion"
      <*> o .:? "skill-index"
    where
      resourceSpecificationP obj =
        let
          resource = do
            bounds <- obj .:? "bounds"
            regen <- obj .:? "regen"
            startVal <- obj .:? "start"
            if isJust bounds || isJust regen || isJust startVal
              then pure $ Just ResourceSpecification
                { resSpecBounds = fromMaybe defaultResBounds bounds
                , resSpecRegen = regen
                , resSpecStartValue = fromMaybe Max startVal
                }
              else pure Nothing
          cooldown = do
            cd <- obj .:? "cooldown"
            case cd of
              Just duration -> pure $ Just ResourceSpecification
                { resSpecBounds = ResBounds 0 duration
                , resSpecRegen = Just $ ResRegen 1
                , resSpecStartValue = Max
                }
              Nothing -> pure Nothing
        in liftA2 (<|>) resource cooldown

data Level = Level
  { levelEntities :: Vector EntityDescription
  } deriving (Eq, Show)

instance FromJSON Level where
  parseJSON = withObject "level" $ \o -> Level <$> o .: "resources"

instantiateEntity
  :: HasAll
       w
       '[EntityCounter, Name, ResBounds, ResRegen, ResAmount, Castable, OnCastCompleted, SkillIndex]
  => EntityDescription
  -> System w (Entity Void)
instantiateEntity desc = do
  ety <- newEntity $ etyName desc
  for_ (etyResourceSpec desc) $ \resSpec -> do
    set ety $ resSpecBounds resSpec
    for_ (resSpecRegen resSpec) $ set ety
    set ety . ResAmount $ case resSpecStartValue resSpec of
      Min          -> resBoundsMin . resSpecBounds $ resSpec
      Max          -> resBoundsMax . resSpecBounds $ resSpec
      Fixed amount -> amount
  for_ (etyCastable desc) $ set ety
  for_ (etyCastCompletion desc) $ set ety . OnCastCompleted
  for_ (etySkillIndex desc) $ set ety
  pure (cast ety)

instantiateLevel
  :: HasAll
       w
       '[EntityCounter, Name, ResBounds, ResRegen, ResAmount, Castable, OnCastCompleted, SkillIndex]
  => Level
  -> System w ()
instantiateLevel level =
  for_ (levelEntities level) instantiateEntity

loadLevel :: MonadIO m => FilePath -> m Level
loadLevel path = liftIO $ decodeFileEither path >>= either throwIO pure
