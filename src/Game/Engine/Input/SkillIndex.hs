module Game.Engine.Input.SkillIndex where

import           Data.Aeson
import           Data.Hashable
import           Data.Text         (Text)

import           Apecs

import           Apecs.EntityIndex

newtype SkillIndex = SkillIndex { skillIndex :: Text } deriving (Eq, Ord, Show, Hashable)
instance Component SkillIndex where type Storage SkillIndex = EntityIndex SkillIndex
instance FromJSON SkillIndex where
  parseJSON = fmap SkillIndex . parseJSON
instance ToJSON SkillIndex where
  toJSON = toJSON . skillIndex
