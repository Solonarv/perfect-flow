module PF.Simulation.Notation where

import GHC.Records (HasField)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import PF.Simulation
import Data.Text (Text)
import Data.Foldable (toList)

byName :: (HasField "name" r n, Ord n) => [r] -> Map n r
byName rs = Map.fromList [(r.name, r) | r <- rs]

chargeResource :: Text -> Int -> Resource
chargeResource name max = Resource {name = name, max = int2Num max, min = 0, current = 0, regen = 0}

sharedCD :: Text -> World -> World
sharedCD cdName w = case maxCd of
  Just m -> w{resources = Map.insert cdName Resource{name = cdName, min = 0, max = m, current = 0, regen = -1} w.resources}
  Nothing -> w
  where
    maxCd = maximum $ Nothing :
      [ Just cd
      | ability <- toList $ w.abilities
      , cost <- ability.cost
      , cost.target == cdName
      , let cd = - cost.amount
      ]

cdCost :: Text -> Number -> Cost
cdCost cdName delay = Cost{target = cdName, amount = -delay, phantom = False}