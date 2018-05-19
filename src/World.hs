{-# LANGUAGE TemplateHaskell #-}
module World
  ( World(..)
  , initWorld
  , System'
  , module Game.Flow.Components
  ) where

import           Apecs

import           Game.Engine.Input.SkillIndex
import           Game.Flow.Components

makeWorld "World" [ ''ResAmount
                  , ''ResBounds
                  , ''ResRegen
                  , ''ResRenderType
                  , ''Castable
                  , ''Casting
                  , ''Name
                  , ''OnCastCompleted
                  , ''DamageDealt
                  , ''Time
                  , ''SkillIndex
                  ]
type System' = System World
