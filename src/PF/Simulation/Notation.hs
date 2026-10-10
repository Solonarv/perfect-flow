module PF.Simulation.Notation where

import GHC.Records (HasField)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import PF.Simulation
import Data.Text (Text)

byName :: (HasField "name" r n, Ord n) => [r] -> Map n r
byName rs = Map.fromList [(r.name, r) | r <- rs]

chargeResource :: Text -> Int -> Resource
chargeResource name max = Resource {name = name, max = max, min = 0, current = 0}