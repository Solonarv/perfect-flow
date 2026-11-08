module PF.Simulation.Level.Consistency (ConsistencyError(..), checkConsistency) where

import PF.Simulation.Level
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Control.Monad (guard)
import qualified Data.Set as Set
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap
import Symbolize (Symbol)

data ConsistencyError
  = UndefinedBuff Symbol
  | UnusedBuff Symbol
  deriving (Eq, Ord, Show)

sguard :: Bool -> a -> Set a
sguard p x | p = Set.singleton x | otherwise = Set.empty

checkConsistency :: Level -> Set ConsistencyError
checkConsistency lvl = let
  mentionedBuffs = buffNamesMentioned lvl
  appliedBuffs = buffNamesApplied lvl
  buffExists b = sguard (b `HashMap.member` lvl.buffs) (UndefinedBuff b)
  buffIsApplied b = sguard (b `Set.member` appliedBuffs) (UnusedBuff b)
  in
    foldMap buffExists mentionedBuffs
    <> foldMap buffIsApplied mentionedBuffs
  -- TODO resource names

buffNamesMentioned :: Level -> Set Symbol
buffNamesMentioned lvl = Set.fromList $
  mapMaybe buffName (collectEffects lvl)
  where
    buffName = \case
      Effect_ApplyBuff what -> Just what
      Effect_RemoveBuff_All what -> Just what
      Effect_RemoveBuff_Newest what -> Just what
      Effect_RemoveBuff_Oldest what -> Just what
      _ -> Nothing

buffNamesApplied :: Level -> Set Symbol
buffNamesApplied lvl = Set.fromList $
  mapMaybe buffName (collectEffects lvl)
  where
    buffName = \case
      Effect_ApplyBuff what -> Just what
      _ -> Nothing

collectEffects :: Level -> List Effect
collectEffects lvl = concatMap unrollEffect $
    abilityEffects lvl.abilities
  <> buffEffects (toList lvl.buffs)

abilityEffects :: [Ability] -> List Effect
abilityEffects abils = concat
  [ concat [abi.on_cast_start, abi.on_cast_finish, abi.on_channel_tick]
  | abi <- abils
  ]

buffEffects :: [Buff] -> List Effect
buffEffects buffs = concat
  [ concat [buff.on_apply, buff.on_tick, buff.on_expire, buff.on_remove]
  | buff <- buffs
  ]

unrollEffect :: Effect -> List Effect
unrollEffect = \case
  e@(Effect_Randomly _ _ what els)
    -> e : foldMap unrollEffect els ++ unrollEffect what
  e@(Effect_Sequence _ sq) -> e : concatMap unrollEffect sq
  e -> [e]