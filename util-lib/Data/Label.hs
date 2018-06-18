module Data.Label where

import           GHC.OverloadedLabels
import           GHC.TypeLits

data Label (sym :: Symbol) = Label

instance KnownSymbol sym => Show (Label sym) where
  showsPrec _ lbl = ('#':) . showString (symbolVal lbl)

instance IsLabel sym (Label sym) where
  fromLabel = Label
