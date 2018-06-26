{-# LANGUAGE NamedFieldPuns #-}
module SDLUI.Render where

import           Control.Applicative
import           Data.Maybe

import           Apecs
import qualified SDL
import qualified SDL.Font                 as Font

import           Apecs.Default
import           Apecs.Extra
import           Apecs.Monad
import           SDL.Extra
import           SDLUI.Components
import           SDLUI.Components.Globals
import           SDLUI.Core

renderUI :: MonadSdlUI m => m ()
renderUI = do
  renderBackground
  renderBoxes
  renderLabels
  presentM

renderBackground :: MonadSdlUI m => m ()
renderBackground = do
  Colored { colorBG } <- liftSdlUI $ fromMaybe (error "no default color set") <$> getFallback
  rdrDrawColor $ setV colorBG
  clearM

renderBoxes :: MonadSdlUI m => m ()
renderBoxes = lcimapM_ @UIWorld $ \(ety, Box bounds) -> do
  Colored { colorBG } <- liftSdlUI $ getUnsafe (cast ety @Colored)
  rdrDrawColor $ setV colorBG
  fillRectM (Just bounds)

renderLabels :: MonadSdlUI m => m ()
renderLabels = lcimapM_ @UIWorld $ \(ety, (Box (Rect p0 outerDims), Label txt)) -> do
  Colored { colorFG, colorBG } <- liftSdlUI $             getUnsafe (cast ety @Colored)
  TxtFont font                 <- liftSdlUI $             getUnsafe (cast ety @TxtFont)
  Align align                  <- liftSdlUI $ getSafe <$> get       (cast ety @Align)
  tex <- renderText (RenderShaded colorBG) colorFG font txt
  texDims <- queryTextureDims tex
  let targetDims = liftA2 min outerDims texDims
      offset = liftA3 doAlign align outerDims targetDims
      dest = offset + p0
  copyM tex Nothing (Just (SDL.Rectangle (SDL.P dest) 0))
