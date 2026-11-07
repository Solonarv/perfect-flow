{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
module PF.Simulation where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Control.Monad.State
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Ord (clamp)
import Data.Foldable
import Data.Maybe
import Debug.Trace (traceM, traceShow, trace, traceShowId)
import qualified Data.Text as Text
import Data.Set (Set)
import Data.Monoid
import qualified Data.Set as Set
import Data.List

import PF.Data.Number
import PF.Data.Named
import PF.Random

import PF.Simulation.Level

data LiveResource = LiveResource
  { prototype :: Resource
  , current :: Number
  }
  deriving (Eq, Ord, Show)
  deriving (Named) via (Prototyped LiveResource)

data LiveBuff = LiveBuff
  { prototype :: Buff
  , remaining :: Number
  }
  deriving (Eq, Ord, Show)
  deriving (Named) via (Prototyped LiveBuff)


data CurrentCast = CurrentCast
  { ability :: Ability
  , progress :: Number
  , nextChannelTick :: Number
  , channeling :: Bool
  }
  deriving (Eq, Ord, Show)

data World = World
  { prototype :: Level
  , resources :: Map Text LiveResource
  , currently_casting :: Maybe CurrentCast
  , pending_effects :: [Effect]
  , buffs :: [LiveBuff]
  , tick :: Int
  , damage :: Int
  , rng :: StdGen
  }
  deriving (Eq, Show)

hydrateWorld :: StdGen -> Level -> World
hydrateWorld rng proto = World
  { prototype = proto
  , rng = rng
  , currently_casting = Nothing
  , pending_effects = []
  , buffs = []
  , tick  = 0
  , damage = 0
  , resources = Map.fromList [(r.name, hydrateResource r) | r <- proto.resources]
  }

hydrateResource :: Resource -> LiveResource
hydrateResource proto = LiveResource
  { prototype = proto
  , current = proto.start
  }

step :: World -> World
step = stepPendingEffects . stepCasting . stepBuffs . stepTime

stepTime :: World -> World
stepTime w = w
  { tick = w.tick+1
  , currently_casting = fmap stepCC w.currently_casting
  , resources = fmap (stepRegen w.tick) w.resources
  , buffs = fmap (\b -> b{remaining = b.remaining-1}) w.buffs
  }
  where
    stepCC cc = let
      castSpeed = calculate cc.ability.name Mod_CastSpeed w.buffs 1
      in cc{progress = cc.progress+castSpeed}
    stepRegen tick res
      | res.regen == 0 = res
      | otherwise
      = res{ current = clamp (res.min, res.max) $ res.current + res.regen }

stepBuffs :: World -> World
stepBuffs w = let
  (removed, remaining) = partition (\b -> b.remaining < 0) w.buffs
  ticking = filter shouldTick remaining
  shouldTick buff = buff.tick_time > 0 && abs (snd $ properFraction $ buff.remaining / buff.tick_time) < 1
  in flip (foldr (\b w' -> addEffects b.name b.on_expire w')) removed
  $ foldr ((\b w' -> addEffects b.name b.on_tick w')) w{buffs = remaining} ticking

stepCasting :: World -> World
stepCasting world
  | Just casting <- world.currently_casting
  = flip execState world do
    -- cast done?
    when (casting.progress >= casting.ability.cast_time  && not casting.channeling) do
      modify $ addEffects casting.ability.name casting.ability.on_cast_finish
      -- traceM $ "finished casting "<> Text.unpack casting.ability.name <> ", adding effects " <> show casting.ability.on_cast_finish
      if isChannelStyle casting.ability.cast_style
        then modify \w->w{currently_casting = Just casting{channeling = True, nextChannelTick = casting.ability.channel_tick}}
        else modify stopCasting
    -- channel done?
    when (casting.channeling
      && casting.progress >= casting.ability.channel_time) do
      modify stopCasting
    -- channel tick?
    -- TODO fix channel ticks
    when (casting.channeling && casting.progress >= casting.nextChannelTick) do
      w <- get
      case payCosts casting.ability.name casting.ability.cost_tick w of
        Nothing -> modify stopCasting
        Just paid -> do
          modify @World $ \w->w{resources=paid}
          modify $ addEffects casting.ability.name casting.ability.on_channel_tick
          modify $ \w ->w{currently_casting = Just casting{nextChannelTick = casting.nextChannelTick + casting.ability.channel_tick}}


  | otherwise = world

addEffects :: Text -> [Effect] -> World -> World
addEffects source effects world = world{pending_effects = fmap (augmentEffect source world.buffs) effects <> world.pending_effects}

augmentEffect :: Text -> [LiveBuff] -> Effect -> Effect
augmentEffect source buffs = \case
  Effect_Damage amount -> Effect_Damage (calculate source Mod_Damage buffs amount)
  Effect_GetResource which amount -> Effect_GetResource which (calculate source (Mod_Gained which) buffs amount)
  any -> any


stepPendingEffects :: World -> World
stepPendingEffects world
  | (effect:pending_effects) <- world.pending_effects
  = stepPendingEffects $ applyEffect effect world{pending_effects}
  | otherwise = world

applyEffect :: Effect -> World -> World
applyEffect = \cases
  (Effect_Damage amount) w -> w{damage = w.damage + floor (amount.value)}
  (Effect_GetResource which amount) w -> w{resources = Map.adjust (getResource amount) which w.resources}
  (Effect_ApplyBuff name) w 
    | Just proto <- Map.lookup name w.prototype.buffs
    -> applyBuff proto w
  (Effect_RemoveBuff_Newest which) w
    | (matched, others) <- partition (\b -> b.prototype.name == which) w.buffs
    , (newest:rest) <- matched -> addEffects newest.prototype.name newest.prototype.on_remove w{buffs = rest ++ others}
  (Effect_RemoveBuff_Oldest which) w
    | (matched, others) <- partition (\b -> b.prototype.name == which) w.buffs
    , Just (rest, oldest) <- unsnoc matched
    -> addEffects oldest.prototype.name oldest.prototype.on_remove w{buffs = rest ++ others}
  (Effect_RemoveBuff_All which) w
    | (removed, others) <- partition (\b -> nameOf b == which) w.buffs
    -> addEffects which (concatMap (.prototype.on_remove) removed) w{buffs = others}
  (Effect_Randomly source p what) w
    | (cond, rng) <- rollChance p w.rng
    , let effect = [what | cond]
    -> addEffects source effect w{rng=rng}

getResource :: Number -> LiveResource -> LiveResource
getResource amount res = res{current = clamp (res.prototype.min, res.prototype.max) (res.current + amount)}

data MayInterrupt = Interrupt_Do | Interrupt_Don't
  deriving (Eq, Ord, Show)

cast :: Text -> MayInterrupt -> World -> World
cast what int w
  | Just ability <- Map.lookup what w.prototype.abilities
  , Just paid <- canCast ability int w
  , w <- w{resources = paid} :: World
  , w <- if ability.cast_style == Free
          then w
          else w{currently_casting = Just (CurrentCast ability 0 0 False)}
  , w <- addEffects ability.name (ability.on_cast_start) w
  = stepPendingEffects w
  | otherwise = w

canCast :: Ability -> MayInterrupt -> World -> Maybe (Map Text LiveResource)
canCast ability int w
  | isNothing w.currently_casting
    || ability.cast_style == Free
    || int == Interrupt_Do
  , Just paid <- payCosts ability.name ability.cost w
  = Just paid
  | otherwise = Nothing

stopCasting :: World -> World
stopCasting w = w{currently_casting = Nothing}

payCosts :: Text -> [Cost] -> World -> Maybe (Map Text LiveResource)
payCosts name costs w = foldM expend w.resources (fmap (calculateModifiedCost name w.buffs) costs)

expend :: Map Text LiveResource -> Cost -> Maybe (Map Text LiveResource)
expend resources cost = do
  res <- Map.lookup cost.target resources
  let newAmount = res.current - cost.amount
  guard (newAmount >= res.min && newAmount <= res.max)
  pure if cost.phantom
    then resources
    else Map.insert cost.target res{current = newAmount} resources

applyBuff :: Buff -> World -> World
applyBuff buff w = let
  (existing, others) = partition (\b -> b.prototype.name == buff.name) w.buffs
  (these, triggers) = if
    | length existing < buff.maxStacks
    -> (hydrateBuff buff : existing, buff.on_apply)
    | Buff_Discard <- buff.stackingMode -> (existing, [])
    | Buff_Override <- buff.stackingMode
    , Just (rest, removed) <- unsnoc existing
    -> (hydrateBuff buff:rest, buff.on_apply ++ removed.prototype.on_remove)
    | Buff_Refresh <- buff.stackingMode
    , refresh <- \b -> b{remaining = max b.remaining buff.duration}
    -> (refresh <$> existing, buff.on_apply)
  in addEffects buff.name triggers w{buffs = these ++ others}

hydrateBuff :: Buff -> LiveBuff
hydrateBuff buff = LiveBuff
  { prototype = buff
  , remaining = buff.duration
  }

-- | Apply all modifiers to the given value, respecting modifier class.
executeModifiers :: [Modifier] -> Number -> Number
executeModifiers mods base = let
  mod2vec mod = case mod.class_ of
    Mod_AddBase -> (Sum mod.magnitude, mempty, mempty, mempty, mempty)
    Mod_Increased -> (mempty, Sum mod.magnitude, mempty, mempty, mempty)
    Mod_More -> (mempty, mempty, 1 + Product mod.magnitude, mempty, mempty)
    Mod_Increased_Invert -> (mempty, mempty, mempty, Sum mod.magnitude, mempty)
    Mod_AddFinal -> (mempty, mempty, mempty, mempty, Sum mod.magnitude)
  (addBase, increased, more, increased_invert, addFinal) = foldMap mod2vec mods
  in (base + getSum addBase) * (1 + getSum increased) / (1 - getSum increased_invert) * getProduct more + getSum addFinal

collectModifiersFor :: Text -> ModifierWhat -> [LiveBuff] -> [Modifier]
collectModifiersFor name what buffs = 
  [ mod
  | buff <- buffs
  , mod <- buff.modifiers
  , mod.what == what
  , Set.null mod.targets || name `Set.member` mod.targets
  ]

calculate :: Text -> ModifierWhat -> [LiveBuff] -> Number -> Number
calculate name what buffs base = executeModifiers (collectModifiersFor name what buffs) base

calculateInt :: Text -> ModifierWhat -> [LiveBuff] -> Int -> Int
calculateInt t w b = floor . calculate t w b . int2Num

calculateModifiedCost :: Text -> [LiveBuff] -> Cost -> Cost
calculateModifiedCost name buffs cost = cost{amount = calculate name (Mod_Cost cost.target) buffs cost.amount}