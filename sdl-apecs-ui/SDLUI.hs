module SDLUI
  ( module SDLUI.Components
  , module SDLUI.Core
  , module SDLUI.Interact
  , uiLoop
  ) where

import           Control.Monad

import           Apecs
import qualified SDL

import           Apecs.Extra
import           SDLUI.Components
import           SDLUI.Core
import           SDLUI.Interact
import           SDLUI.Render

uiLoop :: MonadSdlUI m => m ()
uiLoop = do
  ShouldExit done <- liftSdlUI getGlobal
  unless done $ do
    renderUI
    SDL.waitEvent >>= handleEvent
    uiLoop
