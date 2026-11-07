module PF.Random (module System.Random, rollChance) where

import System.Random
import PF.Data.Number
import Data.Ratio

rollChance :: RandomGen g => Number -> g -> (Bool, g)
rollChance (Number p) g = case uniformR (1, denominator p) g of
  (r, g') -> (r <= numerator p, g')