{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module PF.Levels.ArcaneWizard where

import PF.Simulation
import PF.Simulation.Notation
import PF.Number

initWorld :: World
initWorld = sharedCD "gcd" $ emptyWorld
  { resources = byName
    [ Resource
      { name = "mana"
      , min = 0, max = 1000, current = 1000
      , regen = 5 / 60
      }
    , chargeResource "evocation-charges" 20
    , chargeResource "arcane-missiles-charges" 1
    ]
  , abilities = byName $
    [ Ability
      { name = "evocation"
      , cast_style = Channel
      , cast_time = 0, channel_time = 600, channel_tick = 120
      , on_cast_start = [Effect_RemoveBuff_All "arcane-blast"]
      , on_cast_finish = []
      , on_channel_tick = [Effect_GetResource "mana" 120]
      , cost = [_GCD, Cost "evocation-charges" 20 False]
      , cost_tick = []
      }
    , Ability
      { name = "arcane-missiles"
      , cast_style = Channel
      , cast_time = 0, channel_time = 180, channel_tick = 30
      , on_cast_start = []
      , on_cast_finish = []
      , on_channel_tick = [Effect_Damage 90]
      , cost = [_GCD, Cost "arcane-missiles-charges" 1 False]
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
      { name = "arcane-blast"
      , cast_style = Normal
      , cast_time = 90, channel_time = 0, channel_tick = 0
      , on_cast_start = []
      , on_cast_finish =
        [ Effect_Damage 100
        , Effect_GetResource "evocation-charges" 1
        , missileChance 0.2
        , Effect_ApplyBuff Buff
          { name = "arcane-blast"
          , stackingMode = Buff_Refresh
          , maxStacks = 4
          , original_duration = 3600
          , remaining = 3600
          , on_apply = []
          , on_tick = []
          , on_expire = []
          , on_remove = []
          , tick_time = 0
          , modifiers =
            [ Modifier
              { name = "arcane-blast-damage"
              , targets = ["arcane-blast", "arcane-barrage", "arcane-missiles"]
              , class_ = Mod_Increased
              , what = Mod_Damage
              , magnitude = 0.4
              }
            , Modifier
              { name = "arcane-blast-cost"
              , targets = ["arcane-blast"]
              , class_ = Mod_AddBase
              , what = Mod_Cost "mana"
              , magnitude = 8
              }
            , Modifier
              { name = "arcane-blast-speed"
              , targets = ["arcane-blast"]
              , class_ = Mod_Increased
              , what = Mod_CastSpeed
              , magnitude = 0.3
              }
            , Modifier
              { name = "arcane-blast-gcd"
              , targets = ["arcane-blast"]
              , class_ = Mod_Increased_Invert
              , what = Mod_Cost "gcd"
              , magnitude = -0.3
              }
            ]
          }
        ]
      , on_channel_tick = []
      , cost =
        [_GCD
        , Cost "mana" 12 False
        ]
      , cost_tick = []
      }
    ]
  }

_GCD :: Cost
_GCD = cdCost "gcd" 60

missileChance :: Number -> Effect
missileChance p = Effect_Randomly "arcane-missile-recharge" p (Effect_GetResource "arcane-missiles-charges" 1)