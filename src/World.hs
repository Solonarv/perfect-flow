{-# language TemplateHaskell #-}
{-# language GeneralizedNewtypeDeriving #-}
module World
  ( World(..)
  , initWorld
  , System'
  , module Game.Flow.Components
  ) where

import Data.Monoid

import Apecs

import Game.Flow.Components

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
                  ]
type System' = System World