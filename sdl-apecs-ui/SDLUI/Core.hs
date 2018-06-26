{-# LANGUAGE TemplateHaskell #-}
module SDLUI.Core where

import           Apecs

import           Apecs.Monad
import           SDL.Extra
import           SDLUI.Components
import           SDLUI.Components.Globals

Apecs.makeWorld "UIWorld"
                [ ''ShouldExit
                , ''Box
                , ''Clickable
                , ''Label
                , ''Colored
                , ''Align
                , ''TxtFont
                ]

type MonadSdlUI m = (MonadSystem UIWorld m, MonadFontRender m, MonadRenderer m)

liftSdlUI :: MonadSystem UIWorld m => System UIWorld a -> m a
liftSdlUI = liftSystem @UIWorld
