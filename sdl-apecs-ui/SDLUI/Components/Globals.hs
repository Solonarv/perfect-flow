module SDLUI.Components.Globals where

import           Control.DeepSeq

import           Apecs
import           Apecs.Constant
import qualified SDL

newtype ShouldExit = ShouldExit Bool
instance Monoid ShouldExit where
  mempty = ShouldExit False
  ShouldExit l `mappend` ShouldExit r = ShouldExit $ l || r
instance Component ShouldExit where type Storage ShouldExit = Global ShouldExit

setExit :: Has w ShouldExit => Bool -> System w ()
setExit = setGlobal . ShouldExit
