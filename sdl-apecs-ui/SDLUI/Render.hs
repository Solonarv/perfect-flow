{-# LANGUAGE NamedFieldPuns #-}
module SDLUI.Render where

import           Control.Applicative
import           Data.Maybe

import           Apecs
import qualified SDL
import qualified SDL.Font                 as Font

import           Apecs.Default
import           Apecs.Extra
import           SDLUI.Components
import           SDLUI.Components.Globals

renderUI :: HasAll w [Box, Colored, Label, RenderTarget, TxtFont, Align] => System w ()
renderUI = do
  renderBackground
  renderBoxes
  renderLabels
  RenderTarget win <- getGlobal
  SDL.updateWindowSurface win

renderBackground :: HasAll w [Colored, RenderTarget] => System w ()
renderBackground = do
  RenderTarget win <- getGlobal
  surf <- SDL.getWindowSurface win
  Colored { colorBG } <- fromMaybe (error "no default color set") <$> getFallback
  SDL.surfaceFillRect surf Nothing colorBG

renderBoxes :: HasAll w [Box, Colored, RenderTarget] => System w ()
renderBoxes = cimapM_ $ \(ety, Box bounds) -> do
  RenderTarget win <- getGlobal
  surf <- SDL.getWindowSurface win
  Colored { colorBG } <- getUnsafe (cast ety @Colored)
  SDL.surfaceFillRect surf (Just bounds) colorBG

renderLabels :: HasAll w [Box, Label, RenderTarget, Colored, TxtFont, Align] => System w ()
renderLabels = cimapM_ $ \(ety, (Box (Rect p0 outerDims), Label txt)) -> do
  RenderTarget win <- getGlobal
  surf <- SDL.getWindowSurface win
  Colored { colorFG, colorBG } <- getUnsafe (cast ety @Colored)
  TxtFont font <- getUnsafe (cast ety @TxtFont)
  Align align <- getSafe <$> get (cast ety @Align)
  txtSurf <- Font.shaded font colorFG colorBG txt
  txtDims <- SDL.surfaceDimensions txtSurf
  let targetDims = liftA2 min outerDims txtDims
      offset = liftA3 doAlign align outerDims targetDims
      dest = offset + p0
  SDL.surfaceBlit txtSurf Nothing surf (Just $ SDL.P dest)
  pure ()
