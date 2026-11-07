module PF.Random (module System.Random, rollChance, rollRangePrec) where

import System.Random
import PF.Data.Number
import Data.Ratio

rollChance :: RandomGen g => Number -> g -> (Bool, g)
rollChance (Number p) g = case uniformR (1, denominator p) g of
  (r, g') -> (r <= numerator p, g')

rollRangePrec :: RandomGen g => Number -> Number -> g -> (Number, g)
rollRangePrec (Number hi) (Number p) g = let
  denom = denominator hi * denominator p
  (roll, g') = uniformR (1, denom) g
  in (Number (roll % denom), g')