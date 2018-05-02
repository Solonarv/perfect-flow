{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad
import Data.Functor
import Data.Monoid

import Apecs
import SDL hiding (get)
import qualified SDL

import Data.Text (Text)

newtype ResAmount = ResAmount Int deriving Show
instance Component ResAmount where type Storage ResAmount = Map ResAmount

data ResBounds = ResBounds Int Int deriving Show
instance Component ResBounds where type Storage ResBounds = Map ResBounds

newtype ResRegen = ResRegen Int deriving Show
instance Component ResRegen where type Storage ResRegen = Map ResRegen

data Castable = Castable Int Int deriving Show
instance Component Castable where type Storage Castable = Map Castable

newtype Casting = Casting Int deriving Show
instance Component Casting where type Storage Casting = Unique Casting

data Damage = Damage Int deriving Show
instance Component Damage where type Storage Damage = Map Damage

newtype Name = Name Text deriving Show
instance Component Name where type Storage Name = Map Name

newtype DamageDealt = DamageDealt (Sum Int) deriving (Show, Num, Monoid)
instance Component DamageDealt where type Storage DamageDealt = Global DamageDealt

makeWorld "World" [ ''ResAmount
                  , ''ResBounds
                  , ''ResRegen
                  , ''Castable
                  , ''Casting
                  , ''Name
                  , ''Damage
                  , ''DamageDealt
                  ]
type System' = System World

main :: IO ()
main = do
  initializeAll
  world <- initWorld
  window <- createWindow "Perfect Flow" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  runSystem initializeEntities world

initializeEntities :: System' ()
initializeEntities = do
  setGlobal (DamageDealt 0)
  newEntity (Name "Strike", ResAmount 100, ResBounds 0 100, ResRegen 1, Castable 100 30 )
  pure ()

tick :: Int -> System' ()
tick dT = do
  regenResources
  clampResources
  advanceCasting
  resolveCasting
  where
    regenResources = rmap $ \(ResAmount amt, ResRegen reg) -> ResAmount (amt + reg * dT)
    clampResources = rmap $ \(ResAmount amt, ResBounds lo hi) -> ResAmount (clamp lo hi amt)
    advanceCasting = cmap $ \(Casting progress) -> Casting (progress + dT)
    resolveCasting = cimapM_ $ \(e, (Casting progress, Castable cost casttime)) ->
      when (progress >= casttime) $ do
        destroy $ cast e @Casting
        get (cast e @Damage) >>= \case
          Safe Nothing -> pure ()
          Safe (Just (Damage dmg)) -> modifyGlobal $ mappend @DamageDealt $ fromIntegral dmg


clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = min hi (max x lo)