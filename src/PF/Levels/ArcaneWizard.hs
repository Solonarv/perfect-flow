{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module PF.Levels.ArcaneWizard where

import PF.Simulation.Notation
import PF.Data.Number
import PF.Simulation.Level

level :: Level
level = sharedCD "gcd" $ Level
  { resources =
    [ Resource
      { name = "mana"
      , min = 0, max = 1000, start = 1000
      , regen = 5 / 60
      }
    , Resource
      { name = "evocation.cooldown"
      , min = 0, max = 45, start = 0
      , regen = -1/60
      }
    , Resource
      { name = "power-surge.cooldown"
      , min = 0, max = 90, start = 0
      , regen = -1/60
      }
    , chargeResource "arcane-missiles.active" 1
    ]
  , abilities =
    [ Ability
      { name = "arcane-blast"
      , cast_style = Normal
      , cast_time = 120, channel_time = 0, channel_tick = 0
      , on_cast_start = []
      , on_cast_finish =
        [ Effect_Damage 100
        , missileChance 0.2
        , Effect_ApplyBuff "arcane-blast"
        ]
      , on_channel_tick = []
      , cost =
        [_GCD
        , Cost "mana" 12 False
        ]
      , cost_tick = []
      }
    , Ability
      { name = "arcane-missiles"
      , cast_style = Channel
      , cast_time = 0, channel_time = 180, channel_tick = 30
      , on_cast_start = []
      , on_cast_finish = []
      , on_channel_tick =
        [ Effect_Damage 90
        , Effect_Randomly "arcane-missiles.recharge-evocation" 0.2
          (Effect_GetResource "evocation.cooldown" (-1))
          Nothing
        ]
      , cost = [_GCD, Cost "arcane-missiles.active" 1 False]
      , cost_tick = []
      }
    , Ability
      { name = "arcane-barrage"
      , cast_style = Instant
      , cast_time = 0, channel_time = 0, channel_tick = 0
      , on_cast_start = [Effect_RemoveBuff_All "arcane-blast"]
      , on_cast_finish = [Effect_Damage 180, missileChance 0.4]
      , on_channel_tick = []
      , cost = [_GCD, Cost "mana" 10 False]
      , cost_tick = []
      }
    , Ability
      { name = "evocation"
      , cast_style = Channel
      , cast_time = 0, channel_time = 600, channel_tick = 30
      , on_cast_start = [Effect_RemoveBuff_All "arcane-blast"]
      , on_cast_finish = []
      , on_channel_tick = [Effect_GetResource "mana" 30]
      , cost = [_GCD, Cost "evocation.cooldown" (-45) False]
      , cost_tick = []
      }
    , Ability
      { name = "power-surge"
      , cast_style = Free
      , cast_time = 0, channel_time = 0, channel_tick = 0
      , on_cast_start =
        replicate 4 (Effect_ApplyBuff "arcane-blast") ++
        [ Effect_ApplyBuff "power-surge"
        ]
      , on_cast_finish = [], on_channel_tick = []
      , cost = [Cost "mana" 100 False, Cost "power-surge.cooldown" (-90) False]
      , cost_tick = []
      }
    ]
  , buffs = byName
    [ Buff
      { name = "arcane-blast"
      , stackingMode = Buff_Refresh
      , maxStacks = 4
      , duration = 3600
      , on_apply = []
      , on_tick = []
      , on_expire = []
      , on_remove = []
      , tick_time = 0
      , modifiers =
        [ Modifier
          { name = "arcane-blast.damage"
          , targets = ["arcane-blast", "arcane-barrage", "arcane-missiles"]
          , class_ = Mod_Increased
          , what = Mod_Damage
          , magnitude = 0.4
          }
        , Modifier
          { name = "arcane-blast.cost"
          , targets = ["arcane-blast"]
          , class_ = Mod_AddBase
          , what = Mod_Cost "mana"
          , magnitude = 20
          }
        , Modifier
          { name = "arcane-blast.speed"
          , targets = ["arcane-blast"]
          , class_ = Mod_Increased
          , what = Mod_CastSpeed
          , magnitude = 0.25
          }
        ]
      }
    , Buff
      { name = "power-surge"
      , stackingMode = Buff_Discard
      , maxStacks = 1
      , duration = 15*60
      , on_apply = []
      , on_tick = []
      , on_expire = []
      , on_remove = []
      , tick_time = 0
      , modifiers =
        [ Modifier
          { name = "power-surge.damage"
          , targets = ["arcane-blast", "arcane-barrage", "arcane-missiles"]
          , what = Mod_Damage
          , magnitude = 0.4, class_= Mod_More
          }
        , Modifier
          { name = "power-surge.manacost"
          , targets = ["arcane-blast", "arcane-barrage", "arcane-missiles"]
          , what = Mod_Cost "mana"
          , magnitude = 0.4, class_ = Mod_More
          }
        ]
      }
    ]
  }

_GCD :: Cost
_GCD = cdCost "gcd" 60

missileChance :: Number -> Effect
missileChance p = Effect_Randomly "arcane-missile-recharge" p (Effect_GetResource "arcane-missiles.active" 1) Nothing