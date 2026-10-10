{-# LANGUAGE OverloadedStrings #-}
module PF.Render where

import PF.AppM
import PF.Simulation
import Data.IORef
import Control.Monad.Reader
import Data.Foldable
import Foreign.C
import Linear
import SDL
import Control.Monad
import qualified Data.Text as Text
import qualified SDL.Font as Font



render :: AppM ()
render = do
  rdr <- asks (.renderer)
  rendererDrawColor rdr $= V4 255 255 255 255
  clear rdr
  drawResources
  drawAbilities
  drawCasting
  drawDamage

drawResources :: AppM ()
drawResources = do
  world <- liftIO . readIORef =<< asks (.world)
  for_ (zip [0..] (toList world.resources)) \(i, res) -> do
    rdr <- asks (.renderer)
    let
      width, height, fillWidth :: CInt
      width = 200
      height = 40
      fillLevel = fromIntegral res.current / fromIntegral res.max :: Rational
      fillWidth = round (fromIntegral width * fillLevel)
      minFillWidth = round (fromIntegral width * fromIntegral res.min / fromIntegral res.max :: Rational)
      basePoint = P (V2 420 (10 + 50*i))
    rendererDrawColor rdr $= V4 0 255 0 255
    fillRect rdr (Just $ Rectangle basePoint (V2 fillWidth height))
    when (res.min > 0) do
      rendererDrawColor rdr $= V4 127 127 0 255
      fillRect rdr (Just $ Rectangle basePoint (V2 minFillWidth height))
    rendererDrawColor rdr $= V4 0 0 0 255
    drawRect rdr (Just $ Rectangle basePoint (V2 width height))
    let label = res.name <> " " <> Text.pack (show res.current <> "/" <> show res.max)
    font <- asks (.opensans12)
    labelSurf <- Font.solid font (V4 60 127 240 255) label
    labelTex <- createTextureFromSurface rdr labelSurf
    labelSize <- surfaceDimensions labelSurf
    copy rdr labelTex Nothing (Just $ Rectangle basePoint labelSize)

drawAbilities :: AppM ()
drawAbilities = do
  world <- liftIO . readIORef =<< asks (.world)
  for_ (zip [0..] (toList world.abilities)) \(i, ability) -> do
    rdr <- asks (.renderer)
    let
      width, height :: CInt
      width = 180
      height = 40
      basePoint = P (V2 20 (10 + 50*i))
      rect = Rectangle basePoint (V2 width height)
      color
        | canCast ability Interrupt_Don't world = V4 0 255 0 255
        | canCast ability Interrupt_Do world = V4 255 255 0 255
        | otherwise = V4 255 0 0 255
    rendererDrawColor rdr $= color
    fillRect rdr (Just rect)
    rendererDrawColor rdr $= V4 0 0 0 255
    drawRect rdr (Just rect)
    font <- asks (.opensans12)
    labelSurf <- Font.solid font (V4 60 127 240 255) ability.name
    labelTex <- createTextureFromSurface rdr labelSurf
    labelSize <- surfaceDimensions labelSurf
    copy rdr labelTex Nothing (Just $ Rectangle basePoint labelSize)


drawCasting :: AppM ()
drawCasting = pure ()
drawDamage :: AppM ()
drawDamage = pure ()