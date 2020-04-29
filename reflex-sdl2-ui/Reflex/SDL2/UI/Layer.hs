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
Module      :   Reflex.SDL2.UI.Layer
Description :   A thin wrapper around DynamicWriter(T) and PerformEvent to make drawing things with SDL2 + Reflex easier.
Copyright   :   2020 Nicolas Stamm
License     :   GPL-3.0-or-later
-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.SDL2.UI.Layer where

import Data.Functor
import Data.Monoid
import Foreign.C.Types

import Control.Monad.Reader
import Control.Monad.State
import Data.StateVar as StateVar
import Reflex
import Reflex.SDL2
import qualified SDL

import Misc.AABB

newtype Draw m a = Draw { performDraw :: SDL.Renderer -> m a }
  deriving (Functor, Applicative, Monad, MonadIO) via (ReaderT SDL.Renderer m)
  deriving (Semigroup, Monoid) via (SDL.Renderer -> Ap m a)

type DrawLayer m = Draw (Performable m) ()

layer :: (Reflex t, DynamicWriter t (DrawLayer m) m) => Dynamic t (DrawLayer m) -> m ()
layer = tellDyn

runLayers :: (PerformEvent t m, MonadFix m, MonadIO (Performable m), PostBuild t m)
          => Renderer
          -> DynamicWriterT t (DrawLayer m) m a
          -> m a
runLayers r guest = do
  (a, layers) <- runDynamicWriterT guest
  postBuild <- getPostBuild
  let firstDraw = tag (current layers) postBuild
  performEvent_ $ leftmost [updated layers, firstDraw] <&> \layer -> do
    SDL.rendererDrawColor r $= V4 0 0 0 255
    SDL.clear r
    performDraw layer r
    SDL.present r
  pure a

aabbToRect :: (Ord a, Num a) => AABB V2 a -> Rectangle a
aabbToRect (AABB (P lo) (P hi)) = Rectangle (P lo) (hi - lo)

rendererState :: MonadIO m => (SDL.Renderer -> StateVar s) -> Draw (StateT s m) a -> Draw m a
rendererState sv (Draw inner) = Draw \r -> do
  s <- StateVar.get (sv r)
  (a, s') <- runStateT (inner r) s
  sv r $= s'
  pure a

renderClear, renderPresent :: MonadIO m => Draw m ()
renderClear = Draw \r -> SDL.clear r
renderPresent = Draw \r -> SDL.present r

renderCopy :: MonadIO m => SDL.Texture -> Maybe (AABB V2 CInt) -> Maybe (AABB V2 CInt) -> Draw m ()
renderCopy tex src dst = Draw \r -> SDL.copy r tex (aabbToRect <$> src) (aabbToRect <$> dst)