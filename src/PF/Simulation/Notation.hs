{-# LANGUAGE OverloadedStrings #-}
module PF.Simulation.Notation (
  byName, chargeResource, cdCost, sharedCD, procWeighted,
  module Symbolize, module PF.Data.Number
) where

import GHC.Records (HasField)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Symbolize (Symbol)
import Data.Foldable (toList)
import PF.Simulation.Level
import PF.Data.Number
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Symbolize as Sym

byName :: (HasField "name" r n, Hashable n) => [r] -> HashMap n r
byName rs = HashMap.fromList [(r.name, r) | r <- rs]

chargeResource :: Symbol -> Int -> Resource
chargeResource name max = Resource {name = name, max = int2Num max, min = 0, start = 0, regen = 0}

sharedCD :: Symbol -> Level -> Level
sharedCD cdName w = case maxCd of
  Just m -> w{resources = Resource{name = cdName, min = 0, max = m, start = 0, regen = -1} : w.resources}
  Nothing -> w
  where
    maxCd = maximum $ Nothing :
      [ Just cd
      | ability <- toList $ w.abilities
      , cost <- ability.cost
      , cost.target == cdName
      , let cd = - cost.amount
      ]

cdCost :: Symbol -> Number -> Cost
cdCost cdName delay = Cost{target = cdName, amount = -delay, phantom = False}

procWeighted :: Symbol -> [(Number, Effect)] -> Effect
procWeighted sym opts = go 1 normalized where
  normalized = [(w/total, e, i) | (i, (w, e)) <- zip [0..] opts]
    where total = sum (fst <$> opts)
  symVal = Sym.unintern sym
  symN i = Sym.intern (symVal <> "[" <> show i <> "]")
  go r = \case
    [] -> Effect_Sequence "<empty-sequence>" []
    [(p, e, i)] -> Effect_Randomly (symN i) (p/r) e Nothing
    [(p, e, i), (_, e', _)] -> Effect_Randomly (symN i) (p/r) e (Just e')
    ((p, e, i):xs) -> Effect_Randomly (symN i) (p/r) e (Just (go (r-p) xs))