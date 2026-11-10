{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
module PF.Simulation where

import Symbolize (Symbol)
import qualified Symbolize
import Data.HashMap.Strict (HashMap)
import Control.Monad.State
import Control.Monad
import qualified Data.HashMap.Strict as HashMap
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
import Control.Applicative

data LiveResource = LiveResource
  { prototype :: Resource
  , current :: Number
  }
  deriving (Eq, Ord, Show)
  deriving (Named) via (Prototyped LiveResource)

data LiveBuff = LiveBuff
  { prototype :: Buff
  , remaining :: Number
  , nextTick :: Number
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
  , resources :: HashMap Symbol LiveResource
  , currently_casting :: Maybe CurrentCast
  , pending_effects :: [Effect]
  , buffs :: [LiveBuff]
  , tick :: Int
  , damage :: Int
  , rng :: StdGen
  , procState :: HashMap Symbol Number
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
  , resources = HashMap.fromList [(r.name, hydrateResource r) | r <- proto.resources]
  , procState = HashMap.empty
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
      | res.prototype.regen == 0 = res
      | otherwise
      = let gain = calculate (nameOf res) (Mod_Gained (nameOf res)) w.buffs res.prototype.regen
        in res{ current = clamp (res.prototype.min, res.prototype.max) $ res.current + gain }

stepBuffs :: World -> World
stepBuffs w = let
  (removed, remaining) = partition (\b -> b.remaining < 0) w.buffs
  ticking = [b | b <- remaining, shouldTick b]
  remaining' = map advanceTick remaining
  shouldTick buff = buff.prototype.tick_time > 0 && buff.nextTick <= int2Num w.tick
  advanceTick buff = if shouldTick buff
    then buff{nextTick = buff.nextTick + buff.prototype.tick_time}
    else buff
  in flip (foldr (\b w' -> addEffects (nameOf b) b.prototype.on_expire w')) removed
  $ foldr ((\b w' -> addEffects (nameOf b) b.prototype.on_tick w')) w{buffs = remaining'} ticking

stepCasting :: World -> World
stepCasting world
  | Just casting <- world.currently_casting
  = flip execState world do
    -- cast done?
    when (casting.progress >= casting.ability.cast_time  && not casting.channeling) do
      modify $ addEffects casting.ability.name casting.ability.on_cast_finish
      -- traceM $ "finished casting "<> Symbol.unpack casting.ability.name <> ", adding effects " <> show casting.ability.on_cast_finish
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

addEffects :: Symbol -> [Effect] -> World -> World
addEffects source effects world = world{pending_effects = fmap (augmentEffect source world.buffs) effects <> world.pending_effects}

augmentEffect :: Symbol -> [LiveBuff] -> Effect -> Effect
augmentEffect source buffs = \case
  Effect_Damage amount -> Effect_Damage (calculate source Mod_Damage buffs amount)
  Effect_GetResource which amount -> Effect_GetResource which (calculate source (Mod_Gained which) buffs amount)
  Effect_Randomly src p what els -> Effect_Randomly src (calculate src Mod_Chance buffs p) what els
  any -> any


stepPendingEffects :: World -> World
stepPendingEffects world
  | (effect:pending_effects) <- world.pending_effects
  = stepPendingEffects $ applyEffect effect world{pending_effects}
  | otherwise = world

applyEffect :: Effect -> World -> World
applyEffect = \cases
  (Effect_Damage amount) w -> w{damage = w.damage + floor (amount.value)}
  (Effect_GetResource which amount) w -> w{resources = HashMap.adjust (getResource amount) which w.resources}
  (Effect_ApplyBuff name) w 
    | Just proto <- HashMap.lookup name w.prototype.buffs
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
  (Effect_TrySpend source cost what els) w
    | paid <- payCosts source cost w
    , let effect = maybeToList ((what <$ paid) <|> els)
    , res' <- fromMaybe w.resources paid
    -> addEffects source effect w{resources = res'}
  (Effect_Randomly source p what els) w
    | (cond, w') <- rollProc source p w
    , let effect = if cond then [what] else maybeToList els
    -> addEffects source effect w'
  (Effect_Sequence source sq) w -> addEffects source sq w

getResource :: Number -> LiveResource -> LiveResource
getResource amount res = res{current = clamp (res.prototype.min, res.prototype.max) (res.current + amount)}

data MayInterrupt = Interrupt_Do | Interrupt_Don't
  deriving (Eq, Ord, Show)

cast :: Symbol -> MayInterrupt -> World -> World
cast what int w
  | [ability] <- filter (\a -> nameOf a == what) w.prototype.abilities
  , Just paid <- canCast ability int w
  , w <- w{resources = paid} :: World
  , w <- if ability.cast_style == Free
          then w
          else w{currently_casting = Just (CurrentCast ability 0 0 False)}
  , w <- addEffects ability.name (ability.on_cast_start) w
  = stepPendingEffects w
  | otherwise = w

canCast :: Ability -> MayInterrupt -> World -> Maybe (HashMap Symbol LiveResource)
canCast ability int w
  | isNothing w.currently_casting
    || ability.cast_style == Free
    || int == Interrupt_Do
  , Just paid <- payCosts ability.name ability.cost w
  = Just paid
  | otherwise = Nothing

stopCasting :: World -> World
stopCasting w = w{currently_casting = Nothing}

payCosts :: Symbol -> [Cost] -> World -> Maybe (HashMap Symbol LiveResource)
payCosts name costs w = foldM expend w.resources (fmap (calculateModifiedCost name w.buffs) costs)

expend :: HashMap Symbol LiveResource -> Cost -> Maybe (HashMap Symbol LiveResource)
expend resources cost = do
  res <- HashMap.lookup cost.target resources
  let newAmount = res.current - cost.amount
  guard (newAmount >= res.prototype.min && newAmount <= res.prototype.max)
  pure if cost.phantom
    then resources
    else HashMap.insert cost.target res{current = newAmount} resources

applyBuff :: Buff -> World -> World
applyBuff buff w = let
  (existing, others) = partition (\b -> b.prototype.name == buff.name) w.buffs
  (these, triggers) = if
    | length existing < buff.maxStacks
    -> (hydrateBuff w.tick buff : existing, buff.on_apply)
    | Buff_Discard <- buff.stackingMode -> (existing, [])
    | Buff_Override <- buff.stackingMode
    , Just (rest, removed) <- unsnoc existing
    -> (hydrateBuff w.tick buff:rest, buff.on_apply ++ removed.prototype.on_remove)
    | Buff_Refresh <- buff.stackingMode
    , refresh <- \b -> b{remaining = max b.remaining buff.duration}
    -> (refresh <$> existing, buff.on_apply)
  in addEffects buff.name triggers w{buffs = these ++ others}

hydrateBuff :: Int -> Buff -> LiveBuff
hydrateBuff now buff = LiveBuff
  { prototype = buff
  , remaining = buff.duration
  , nextTick = buff.tick_time + int2Num now
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

collectModifiersFor :: Symbol -> ModifierWhat -> [LiveBuff] -> [Modifier]
collectModifiersFor name what buffs = 
  [ mod
  | buff <- buffs
  , mod <- buff.prototype.modifiers
  , mod.what == what
  , Set.null mod.targets || name `Set.member` mod.targets
  ]

calculate :: Symbol -> ModifierWhat -> [LiveBuff] -> Number -> Number
calculate name what buffs base = executeModifiers (collectModifiersFor name what buffs) base

calculateInt :: Symbol -> ModifierWhat -> [LiveBuff] -> Int -> Int
calculateInt t w b = floor . calculate t w b . int2Num

calculateModifiedCost :: Symbol -> [LiveBuff] -> Cost -> Cost
calculateModifiedCost name buffs cost = cost{amount = calculate name (Mod_Cost cost.target) buffs cost.amount}

rollProc :: Symbol -> Number -> World -> (Bool, World)
rollProc name p w = let
  state = fromMaybe 0 $ HashMap.lookup name w.procState
  (roll, rng') = rollRangePrec 2 (p/100) w.rng
  total = state + p
  happens = roll <= total
  total' = if happens then total - roll else total
  procState = HashMap.insert name total' w.procState
  in (happens, w{rng = rng', procState = procState})