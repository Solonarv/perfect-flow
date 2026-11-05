{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PF.Simulation where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Control.Monad.State
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Ord (clamp)
import Data.Foldable
import Data.Maybe
import Debug.Trace (traceM, traceShow)
import qualified Data.Text as Text
import Data.Ratio
import Data.Set (Set)
import Data.Monoid
import qualified Data.Set as Set

newtype Number = Number { value :: Ratio Int }
  deriving newtype (Eq, Ord, Show, Num, Fractional, Real, RealFrac)

int2Num :: Int -> Number
int2Num x = Number (x%1)

data Resource = Resource
  { name :: Text
  , current, min, max :: Number
  , regen :: Number
  }
  deriving (Eq, Ord, Show)

data CastStyle = Free | Instant | Normal | Channel | CastChannel
  deriving (Eq, Ord, Show)

isChannelStyle :: CastStyle -> Bool
isChannelStyle = \case Channel; CastChannel -> True; _ -> False

data Ability = Ability
  { name :: Text
  , cast_style :: CastStyle
  , cast_time :: Number
  , channel_time, channel_tick :: Number
  , on_cast_start, on_cast_finish, on_channel_tick :: [Effect]
  , cost, cost_tick :: [Cost]
  }
  deriving (Eq, Ord, Show)

data Cost = Cost
  { target :: Text
  , amount :: Number
  , phantom :: Bool  -- ^ if true, don't actually subtract the resource, just check if there'd be enough
  }
  deriving (Eq, Ord, Show)

data Effect = Effect_Damage Number | Effect_GetResource Text Number | Effect_ApplyBuff Buff
  deriving (Eq, Ord, Show)

data ModifierClass = Mod_AddBase | Mod_Increased | Mod_More | Mod_AddFinal
  deriving (Eq, Ord, Show)

data ModifierWhat = Mod_Damage | Mod_Cost Text | Mod_CastSpeed | Mod_Max | Mod_Regen | Mod_Gained Text
  deriving (Eq, Ord, Show)

data Modifier = Modifier
  { name :: Text
  , targets :: Set Text
  , class_ :: ModifierClass
  , what :: ModifierWhat
  , magnitude :: Number
  }
  deriving (Eq, Ord, Show)

data BuffStackingMode
  = Buff_Discard      -- ^ Newer buff isn't applied
  | Buff_Override     -- ^ Older buff is removed
  | Buff_Refresh      -- ^ Older buff stays, duration reset
  | Buff_Separate     -- ^ Both buffs stay
  | Buff_RefreshStack -- ^ Both buffs stay, older buff's duration reset
  deriving (Eq, Ord, Show)

data Buff = Buff
  { name :: Text
  , stackingMode :: BuffStackingMode
  , maxStacks :: Int
  , original_duration, remaining :: Number
  , on_apply, on_tick, on_expire, on_remove :: [Effect]
  , tick_time :: Number
  , modifiers :: [Modifier]
  }
  deriving (Eq, Ord, Show)


data CurrentCast = CurrentCast
  { ability :: Ability
  , progress :: Number
  , channel_ticks :: Int
  , channeling :: Bool
  }
  deriving (Eq, Ord, Show)

data World = World
  { resources :: Map Text Resource
  , abilities :: Map Text Ability
  , currently_casting :: Maybe CurrentCast
  , pending_effects :: [Effect]
  , buffs :: [Buff]
  , tick :: Int
  , damage :: Int
  }
  deriving (Eq, Ord, Show)

emptyWorld :: World
emptyWorld = World
  { resources = Map.empty
  , abilities = Map.empty
  , currently_casting = Nothing
  , pending_effects = []
  , buffs = []
  , tick = 0
  , damage = 0
  }

step :: World -> World
step = stepPendingEffects . stepCasting . stepTime

stepTime :: World -> World
stepTime w = w
  { tick = w.tick+1
  , currently_casting = fmap stepCC w.currently_casting
  , resources = fmap (stepRegen w.tick) w.resources
  }
  where
    stepCC cc = let
      castSpeed = calculate cc.ability.name Mod_CastSpeed w.buffs 1
      in cc{progress = cc.progress+castSpeed}
    stepRegen tick res
      | res.regen == 0 = res
      | otherwise
      = res{ current = clamp (res.min, res.max) $ res.current + res.regen }


stepCasting :: World -> World
stepCasting world
  | Just casting <- world.currently_casting
  = flip execState world do
    -- cast done?
    when (casting.progress >= casting.ability.cast_time) do
      modify $ addEffects casting.ability.name casting.ability.on_cast_finish
      -- traceM $ "finished casting "<> Text.unpack casting.ability.name <> ", adding effects " <> show casting.ability.on_cast_finish
      if isChannelStyle casting.ability.cast_style
        then modify \w->w{currently_casting = Just casting{channeling= True}}
        else modify stopCasting
    -- channel done?
    when (isChannelStyle casting.ability.cast_style
      && casting.progress >= casting.ability.channel_time) do
      modify stopCasting
    -- channel tick?
    let targetChannelTicks = floor (casting.progress / max 1 casting.ability.channel_tick)
    when (isChannelStyle casting.ability.cast_style && targetChannelTicks > casting.channel_ticks) do
      w <- get
      let ticksToDo = targetChannelTicks - casting.channel_ticks
          boostedCost = concat $ replicate ticksToDo casting.ability.cost_tick
      case payCosts boostedCost w of
        Nothing -> modify stopCasting
        Just paid -> do
          modify $ \w->w{resources=paid}
          modify $ addEffects casting.ability.name (concat $ replicate ticksToDo casting.ability.on_channel_tick)


  | otherwise = world

addEffects :: Text -> [Effect] -> World -> World
addEffects source effects world = world{pending_effects = fmap (augmentEffect source world.buffs) effects <> world.pending_effects}

augmentEffect :: Text -> [Buff] -> Effect -> Effect
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
  (Effect_ApplyBuff buff) w -> let (buffs, triggers) = applyBuff w buff in addEffects buff.name triggers w{buffs=buffs}

getResource :: Number -> Resource -> Resource
getResource amount res = res{current = clamp (res.min, res.max) (res.current + amount)}

data MayInterrupt = Interrupt_Do | Interrupt_Don't
  deriving (Eq, Ord, Show)

cast :: Text -> MayInterrupt -> World -> World
cast what int w
  | Just ability <- Map.lookup what w.abilities
  , Just paid <- canCast ability int w
  , w <- w{resources = paid}
  , w <- if ability.cast_style == Free
          then w
          else w{currently_casting = Just (CurrentCast ability 0 w.tick False)}
  , w <- addEffects ability.name (ability.on_cast_start) w
  = stepPendingEffects w
  | otherwise = w

canCast :: Ability -> MayInterrupt -> World -> Maybe (Map Text Resource)
canCast ability int w
  | isNothing w.currently_casting
    || ability.cast_style == Free
    || int == Interrupt_Do
  , Just paid <- payCosts ability.cost w
  = Just paid
  | otherwise = Nothing

stopCasting :: World -> World
stopCasting w = w{currently_casting = Nothing}

payCosts :: [Cost] -> World -> Maybe (Map Text Resource)
payCosts costs w = foldM expend w.resources costs

expend :: Map Text Resource -> Cost -> Maybe (Map Text Resource)
expend resources cost = do
  res <- Map.lookup cost.target resources
  let newAmount = res.current - cost.amount
  guard (newAmount >= res.min && newAmount <= res.max)
  pure if cost.phantom
    then resources
    else Map.insert cost.target res{current = newAmount} resources

applyBuff :: World -> Buff -> ([Buff], [Effect])
applyBuff w b = error "TODO"

-- | Apply all modifiers to the given value, respecting modifier class.
executeModifiers :: [Modifier] -> Number -> Number
executeModifiers mods base = let
  mod2vec mod = case mod.class_ of
    Mod_AddBase -> (Sum mod.magnitude, mempty, mempty, mempty)
    Mod_Increased -> (mempty, Sum mod.magnitude, mempty, mempty)
    Mod_More -> (mempty, mempty, 1 + Product mod.magnitude, mempty)
    Mod_AddFinal -> (mempty, mempty, mempty, Sum mod.magnitude)
  (addBase, increased, more, addFinal) = foldMap mod2vec mods
  in (base + getSum addBase) * (1 + getSum increased) * getProduct more + getSum addFinal

collectModifiersFor :: Text -> ModifierWhat -> [Buff] -> [Modifier]
collectModifiersFor name what buffs = 
  [ mod
  | buff <- buffs
  , mod <- buff.modifiers
  , mod.what == what
  , Set.null mod.targets || name `Set.member` mod.targets
  ]

calculate :: Text -> ModifierWhat -> [Buff] -> Number -> Number
calculate name what buffs base = executeModifiers (collectModifiersFor name what buffs) base

calculateInt :: Text -> ModifierWhat -> [Buff] -> Int -> Int
calculateInt t w b = floor . calculate t w b . int2Num