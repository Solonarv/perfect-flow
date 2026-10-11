{-# LANGUAGE OverloadedStrings #-}
module PF.Levels.ArcaneWizard where

import PF.Simulation
import PF.Simulation.Notation

initWorld :: World
initWorld = emptyWorld
  { resources = byName
    [ Resource
      { name = "mana"
      , min = 0, max = 1000, current = 1000
      }
    , chargeResource "evocation-charges" 20
    , (chargeResource "blast0" 1){current=1}
    , chargeResource "blast1" 1
    , chargeResource "blast2" 1
    , chargeResource "blast3" 1
    , chargeResource "blast4" 1
    ]
  , abilities = byName $
    [ Ability
      { name = "evocation"
      , cast_style = Channel
      , cast_time = 0, channel_time = 600, channel_tick = 120
      , on_cast_start = reset_blast
      , on_cast_finish = []
      , on_channel_tick = [Effect_GetResource "mana" 120]
      , cost = [Cost "evocation-charges" 20 False]
      , cost_tick = []
      }
    , Ability
      { name = "arcane-barrage"
      , cast_style = Instant
      , cast_time = 0, channel_time = 0, channel_tick = 0
      , on_cast_start = reset_blast
      , on_cast_finish = [Effect_Damage 180]
      , on_channel_tick = []
      , cost = [Cost "mana" 10 False]
      , cost_tick = []
      }
    ] ++ [ Ability
      { name = this
      , cast_style = Normal
      , cast_time = 75, channel_time = 0, channel_tick = 0
      , on_cast_start = []
      , on_cast_finish =
        [ Effect_Damage damage
        , Effect_GetResource next 1
        , Effect_GetResource "evocation-charges" 1
        ] ++ [Effect_GetResource this (-1) | this /= "blast4"]
      , on_channel_tick = []
      , cost =
        [ Cost this 1 True
        , Cost "mana" mana False
        ]
      , cost_tick = []
      }
      | (this, next, damage, mana) <-
      [ ("blast0", "blast1", 100, 8)
      , ("blast1", "blast2", 140, 12)
      , ("blast2", "blast3", 190, 16)
      , ("blast3", "blast4", 250, 20)
      , ("blast4", "dummy", 310, 24)
      ]
    ]
  }

reset_blast :: [Effect]
reset_blast = [Effect_GetResource "blast0" 1] ++ [Effect_GetResource b (-1) | b <- ["blast1", "blast2", "blast3", "blast4"]]