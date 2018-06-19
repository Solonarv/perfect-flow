module Data.Ord.Extra where

infix 4 `between`
between :: Ord a => a -> (a, a) -> Bool
between x (lo, hi) = if lo <= hi then lo <= x && x <= hi else lo >= x && x >= hi

infix 4 `between0`
between0 :: (Ord a, Num a) => a -> a -> Bool
between0 x hi = between x (0, hi)
