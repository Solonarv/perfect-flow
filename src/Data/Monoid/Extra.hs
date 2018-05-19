module Data.Monoid.Extra where

import           Control.Applicative
import           Data.Semigroup

import           Control.Monad.Trans.Class

newtype LiftMonoid f a = LiftMonoid { unLiftMonoid :: f a } deriving (Eq, Ord, Show, Functor, Applicative, Monad, Foldable, Traversable)

instance (Applicative f, Semigroup a) => Semigroup (LiftMonoid f a) where
  (<>) = liftA2 (<>)
instance (Applicative f, Monoid a) => Monoid (LiftMonoid f a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance MonadTrans LiftMonoid where
  lift = LiftMonoid
