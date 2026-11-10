{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module PF.Levels.Corruptor where
import PF.Simulation.Notation
import PF.Simulation.Level

level :: Level
level = sharedCD "gcd" Level
  { resources =
    [ Resource
      { name = "mana"
      , min = 0, max = 1000, start = 1000
      , regen = 1/60
      }
    , Resource
      { name = "void-shard"
      , min = 0, max = 3, start = 0
      , regen = 0
      }
    ]
  , abilities =
    [ Ability
      { name = "agony-jolt"
      , cast_style = Normal
      , cast_time = 120
      , channel_time = 0, channel_tick = 0
      , on_cast_start = [Effect_RemoveBuff_All "corruption.free-agony-jolt"]
      , on_channel_tick = []
      , cost_tick = []
      , cost = [_GCD, Cost "mana" 80 False]
      , on_cast_finish =
        [Effect_Damage 100, shardChance "agony-jolt.shard" 0.2]
      }
    , Ability
      { name = "corruption"
      , cast_style = Normal
      , cast_time = 120
      , channel_time = 0, channel_tick = 0
      , on_cast_start = [], on_channel_tick = []
      , cost_tick = []
      , cost = [_GCD, Cost "mana" 132 False]
      , on_cast_finish =
        [Effect_ApplyBuff "corruption"]
      }
    , Ability
      { name = "void-infection"
      , cast_style = Instant
      , cast_time = 0, channel_time = 0, channel_tick = 0
      , on_cast_start = [], on_channel_tick = []
      , cost_tick = []
      , cost = [_GCD, Cost "mana" 150 False]
      , on_cast_finish = [Effect_ApplyBuff "void-infection"]
      }
    , Ability
      { name = "soul-barrage"
      , cast_style = Channel
      , cast_time = 0, channel_time = 120, channel_tick = 60
      , on_cast_start = [Effect_Damage 200, Effect_ApplyBuff "soul-barrage"]
      , on_channel_tick = [Effect_Damage 200, Effect_ApplyBuff "soul-barrage"]
      , cost_tick = [Cost "void-shard" 1 False]
      , cost = [_GCD, Cost "void-shard" 1 False]
      , on_cast_finish = []
      }
    , Ability
      { name = "void-tap"
      , cast_style = Free
      , cast_time = 0, channel_time = 0, channel_tick = 0
      , on_cast_start = [Effect_GetResource "mana" 325]
      , on_channel_tick = [], cost_tick = []
      , cost = [_GCD, Cost "void-shard" 1 False]
      , on_cast_finish = []
      }
    ]
  , buffs = byName
    [ Buff
      { name = "void-infection"
      , stackingMode = Buff_Override
      , maxStacks = 1
      , duration = 900
      , on_apply = []
      , on_remove = [Effect_RemoveBuff_All "void-infection.power"]
      , on_expire = [Effect_RemoveBuff_All "void-infection.power"]
      , tick_time = 60
      , on_tick =
        [ Effect_ApplyBuff "void-infection.power"
        , Effect_Damage 20
        , shardChance "void-infection.shard" 0.02
        ]
      , modifiers = []
      }
    , Buff
      { name = "void-infection.power"
      , stackingMode = Buff_Refresh
      , maxStacks = 15
      , duration = 900
      , on_apply = []
      , on_remove = []
      , on_expire = []
      , tick_time = 0
      , on_tick = []
      , modifiers =
        [ Modifier
          { name = "void-infection.power.damage"
          , targets = ["void-infection"]
          , class_ = Mod_AddBase
          , what = Mod_Damage
          , magnitude = 5
          }
        , Modifier
          { name = "void-infection.power.shard"
          , targets = ["void-infection.shard"]
          , class_ = Mod_Increased
          , what = Mod_Chance
          , magnitude = 0.1
          }
        ]
      }
    , Buff
      { name = "soul-barrage"
      , stackingMode = Buff_Refresh
      , maxStacks = 2
      , duration = 90
      , on_apply = []
      , on_remove = []
      , on_expire = []
      , tick_time = 0
      , on_tick = []
      , modifiers =
        [ Modifier
          { name = "soul-barrage.damage"
          , targets = ["soul-barrage"]
          , what = Mod_Damage
          , class_ = Mod_More
          , magnitude = 0.75
          }
        ]
      }
    , Buff
      { name = "corruption"
      , stackingMode = Buff_Override
      , maxStacks = 1
      , duration = 21*60
      , on_apply = [], on_remove = [], on_expire = []
      , tick_time = 3*60
      , on_tick =
        [ Effect_Damage 20
        , shardChance "corruption.shard" $ 0.02
        , Effect_Randomly "corruption.free-agony-jolt" 0.2
          (Effect_ApplyBuff "corruption.free-agony-jolt")
          Nothing
        ]
      , modifiers = []
      }
    , Buff
      { name = "corruption.free-agony-jolt"
      , stackingMode = Buff_Override
      , maxStacks = 1
      , duration = 10*60
      , on_apply = [], on_remove = [], on_expire = []
      , tick_time = 0, on_tick = []
      , modifiers =
        [ Modifier
          { name = "corruption.free-agony-jolt.speed"
          , targets = ["agony-jolt"]
          , what = Mod_CastSpeed
          , class_ = Mod_AddFinal
          , magnitude = 120
          }
        , Modifier
          { name = "corruption.free-agony-jolt.cost"
          , targets = ["agony-jolt"]
          , what = Mod_Cost "mana"
          , class_ = Mod_More
          , magnitude = -1
          }
        , Modifier
          { name = "corruption.free-agony-jolt.damage"
          , targets = ["agony-jolt"]
          , what = Mod_Damage
          , class_ = Mod_More
          , magnitude = 1
          }
        ]
      }
    ]
  }

_GCD :: Cost
_GCD = cdCost "gcd" 60

shardChance :: Symbol -> Number -> Effect
shardChance src p = Effect_Randomly src p (Effect_GetResource "void-shard" 1) Nothing