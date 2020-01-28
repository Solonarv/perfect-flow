{-
    Copyright: 2020 Nicolas Stamm

    This file is part of Perfect Flow.

    Perfect Flow is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Perfect Flow is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Perfect Flow.  If not, see <https://www.gnu.org/licenses/>.
-}
{-|
Module      :   Misc.AABB
Description :   Simple axis-aligned bounding boxes - a Cartesian product of closed intervals
Copyright   :   2020 Nicolas Stamm
License     :   GPL-3.0-or-later
-}
{-# LANGUAGE PatternSynonyms #-}
module Misc.AABB where

import Control.Applicative
import Control.Monad

import Linear.Vector
import Linear.Affine
import Linear.V2

data AABB v a = MkAABB !(Point v a) !(Point v a)
  deriving (Eq, Ord, Show)

instance (Applicative v, Ord a) => Semigroup (AABB v a) where
  MkAABB l1 h1 <> MkAABB l2 h2 = MkAABB (liftA2 min l1 l2) (liftA2 max h1 h2)

mkAABB :: (Ord a, Applicative v) => Point v a -> Point v a -> AABB v a
mkAABB p q = MkAABB (liftA2 min p q) (liftA2 max p q)

unAABB :: AABB v a -> (Point v a, Point v a)
unAABB (MkAABB lo hi) = (lo, hi)

pattern AABB :: (Ord a, Applicative v) => () => Point v a -> Point v a -> AABB v a
pattern AABB lo hi <- MkAABB lo hi
  where AABB v1 v2 = mkAABB v1 v2

isEmptyAABB :: (Ord a, Applicative v, Foldable v) => AABB v a -> Bool
isEmptyAABB (MkAABB lo hi) = or (liftA2 (>) lo hi)

infix 4 `inAABB`
inAABB :: (Ord a, Applicative v, Foldable v) => Point v a -> AABB v a -> Bool
inAABB p (AABB lo hi) = and $ liftA3 (\c l h -> l <= c && c <= h) p lo hi

infix 4 `posWithin`
posWithin :: (Fractional (v a), Num a, Ord a, Additive v, Applicative v, Foldable v)
          => Point v a -> AABB v a -> Maybe (Point v a)
posWithin p bb@(AABB lo hi) = do
  let mid = (lo + hi) / 2
      rel = p .-. mid
  guard (p `inAABB` bb)
  pure (P (rel / aabbExtent bb))

translateAABB :: (Num a, Additive v) => v a -> AABB v a -> AABB v a
translateAABB d (MkAABB lo hi) = MkAABB (lo .+^ d) (hi .+^ d)

aabbExtent :: (Num a, Additive v) => AABB v a -> v a
aabbExtent (MkAABB lo hi) = hi .-. lo

zeroAABB :: (Num a, Additive v) => AABB v a
zeroAABB = MkAABB zero zero