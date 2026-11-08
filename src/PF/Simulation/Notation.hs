module PF.Simulation.Notation where

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