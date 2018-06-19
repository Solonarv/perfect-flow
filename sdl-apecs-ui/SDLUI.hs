{-# LANGUAGE TemplateHaskell #-}
module SDLUI
  ( module SDLUI.Components
  , module SDLUI.Interact
  , uiLoop
  , uiComponents
  , makeWorldWithUI
  ) where

import           Control.Monad

import           Language.Haskell.TH

import           Apecs
import qualified SDL

import           Apecs.Extra
import           SDLUI.Components
import           SDLUI.Interact
import           SDLUI.Render

uiLoop :: HasAll w [ShouldExit, Box, Clickable, Label, Colored, Align, RenderTarget, TxtFont] => System w ()
uiLoop = do
  ShouldExit done <- getGlobal
  unless done $ do
    renderUI
    SDL.waitEvent >>= handleEvent
    uiLoop

uiComponents :: [Name]
uiComponents =
  [ ''ShouldExit
  , ''Box
  , ''Clickable
  , ''Label
  , ''Colored
  , ''Align
  , ''RenderTarget
  , ''TxtFont
  ]

makeWorldWithUI :: String -> [Name] -> DecsQ
makeWorldWithUI name comps = Apecs.makeWorld name (comps ++ uiComponents)
