module PF.Simulation where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Control.Monad.State
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Ord (clamp)
import Data.Foldable
import Data.Maybe

data Resource = Resource
  { name :: Text
  , current, min, max :: Int
  }
  deriving (Eq, Ord, Show)

data CastStyle = Free | Instant | Normal | Channel | CastChannel
  deriving (Eq, Ord, Show)

isChannelStyle :: CastStyle -> Bool
isChannelStyle = \case Channel; CastChannel -> True; _ -> False

data Ability = Ability
  { name :: Text
  , cast_style :: CastStyle
  , cast_time :: Int
  , channel_time, channel_tick :: Int
  , on_cast_start, on_cast_finish, on_channel_tick :: [Effect]
  , cost, cost_tick :: [Cost]
  }
  deriving (Eq, Ord, Show)

data Cost = Cost
  { target :: Text
  , amount :: Int
  , phantom :: Bool  -- ^ if true, don't actually subtract the resource, just check if there'd be enough
  }
  deriving (Eq, Ord, Show)

data Effect = Effect_Damage Int | Effect_GetResource Text Int
  deriving (Eq, Ord, Show)

data CurrentCast = CurrentCast
  { ability :: Ability
  , progress :: Int  -- ^ negative timer means channeling, positive means casting
  }
  deriving (Eq, Ord, Show)

data World = World
  { resources :: Map Text Resource
  , abilities :: Map Text Ability
  , currently_casting :: Maybe CurrentCast
  , pending_effects :: [Effect]
  , tick :: Int
  , damage :: Int
  }
  deriving (Eq, Ord, Show)

step :: World -> World
step = stepPendingEffects . stepCasting . stepTime

stepTime :: World -> World
stepTime w = w{ tick = w.tick+1, currently_casting = fmap stepCC w.currently_casting}
  where stepCC cc = cc{progress = cc.progress + signum(cc.progress)}

stepCasting :: World -> World
stepCasting world
  | Just casting <- world.currently_casting
  = flip execState world do
    -- cast done?
    when (casting.progress >= casting.ability.cast_time) do
      modify $ addEffects casting.ability.on_cast_finish
      if isChannelStyle casting.ability.cast_style
        then modify \w->w{currently_casting = Just casting{progress= -1}}
        else modify stopCasting
    -- channel done?
    when (isChannelStyle casting.ability.cast_style
      && -casting.progress >= casting.ability.channel_time) do
      modify stopCasting
    -- channel tick?
    when (isChannelStyle casting.ability.cast_style
      && casting.progress `mod` casting.ability.channel_tick == 0) do
      w <- get
      case payCosts casting.ability.cost_tick w of
        Nothing -> modify stopCasting
        Just w' -> do
          put w'
          modify $ addEffects casting.ability.on_channel_tick

  | otherwise = world

addEffects :: [Effect] -> World -> World
addEffects effects world = world{pending_effects = effects <> world.pending_effects}

stepPendingEffects :: World -> World
stepPendingEffects world
  | (effect:pending_effects) <- world.pending_effects = applyEffect effect world{pending_effects}
  | otherwise = world

applyEffect :: Effect -> World -> World
applyEffect = \cases
  (Effect_Damage amount) w -> w{damage = w.damage + amount}
  (Effect_GetResource which amount) w -> w{resources = Map.adjust (getResource amount) which w.resources}

getResource :: Int -> Resource -> Resource
getResource amount res = res{current = clamp (res.min, res.max) (res.current + amount)}

data MayInterrupt = Interrupt_Do | Interrupt_Don't
  deriving (Eq, Ord, Show)

cast :: Text -> MayInterrupt -> World -> World
cast what int w
  | Just ability <- Map.lookup what w.abilities
  , Just w <- payCosts ability.cost w
  , isNothing w.currently_casting
    || ability.cast_style == Free
    || int == Interrupt_Do
  , w <- if ability.cast_style == Free
          then w
          else w{currently_casting = Just (CurrentCast ability 0)}
  , w <- addEffects (ability.on_cast_start) w
  = stepPendingEffects w
  | otherwise = w

stopCasting :: World -> World
stopCasting w = w{currently_casting = Nothing}

payCosts :: [Cost] -> World -> Maybe World
payCosts costs w = fmap (\res->w{resources=res}) (foldlM expend w.resources costs)

expend :: Map Text Resource -> Cost -> Maybe (Map Text Resource)
expend resources cost = do
  res <- Map.lookup cost.target resources
  let newAmount = res.current - cost.amount
  guard (newAmount >= res.min && newAmount <= res.max)
  pure if cost.phantom
    then resources
    else Map.insert cost.target res{current = newAmount} resources