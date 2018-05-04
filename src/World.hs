{-# language TemplateHaskell #-}
{-# language GeneralizedNewtypeDeriving #-}
module World where

import Data.Monoid

import Apecs

import Data.Text

newtype ResAmount = ResAmount { getResAmount :: Double } deriving Show
instance Component ResAmount where type Storage ResAmount = Map ResAmount

data ResBounds = ResBounds { resBoundsMin :: Double, resBoundsMax :: Double } deriving Show
instance Component ResBounds where type Storage ResBounds = Map ResBounds

newtype ResRegen = ResRegen { getResRegen :: Double } deriving Show
instance Component ResRegen where type Storage ResRegen = Map ResRegen

data Castable = Castable { castCost :: Double, castTime :: Double } deriving Show
instance Component Castable where type Storage Castable = Map Castable

newtype Casting = Casting { castingProgress :: Double } deriving Show
instance Component Casting where type Storage Casting = Unique Casting

data Damage = Damage { getDamage :: Double } deriving Show
instance Component Damage where type Storage Damage = Map Damage

newtype Name = Name { getName :: Text } deriving (Show, Eq)
instance Component Name where type Storage Name = Map Name

newtype DamageDealt = DamageDealt { getDamageDealt :: Sum Double } deriving (Show, Monoid)
instance Component DamageDealt where type Storage DamageDealt = Global DamageDealt

newtype Time = Time { getTime :: Sum Double } deriving (Show, Monoid)
instance Component Time where type Storage Time = Global Time

makeWorld "World" [ ''ResAmount
                  , ''ResBounds
                  , ''ResRegen
                  , ''Castable
                  , ''Casting
                  , ''Name
                  , ''Damage
                  , ''DamageDealt
                  , ''Time
                  ]
type System' = System World