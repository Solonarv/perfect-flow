{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PF.Data.Number (Number(..), int2Num, (%)) where

import Data.Ratio

newtype Number = Number { value :: Ratio Int }
  deriving newtype (Eq, Ord, Show, Num, Fractional, Real, RealFrac)

int2Num :: Int -> Number
int2Num x = Number (x%1)