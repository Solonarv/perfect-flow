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
import Control.Lens
import Data.Maybe



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
        | isJust $ canCast ability Interrupt_Don't world = V4 0 255 0 255
        | isJust $ canCast ability Interrupt_Do world = V4 255 255 0 255
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
drawCasting = do
  world <- liftIO . readIORef =<< asks (.world)
  for_ world.currently_casting \casting -> do
    rdr <- asks (.renderer)
    let
      ability = casting.ability
      width = 240
      height = 60
      basePoint = P (V2 200 380)
      color = V4 0 255 0 255
      progress
        | casting.progress < 0 = ability.channel_time + casting.progress
        | otherwise = casting.progress
      max_progress
        | casting.progress < 0 = ability.channel_time
        | otherwise = max 1 ability.cast_time
      fillWidth = round (fromIntegral width * fromIntegral progress / fromIntegral max_progress :: Rational)
    rendererDrawColor rdr $= V4 0 255 0 255
    fillRect rdr (Just $ Rectangle basePoint (V2 fillWidth height))
    rendererDrawColor rdr $= V4 0 0 0 255
    drawRect rdr (Just $ Rectangle basePoint (V2 width height))
    let label = ability.name <> " " <> Text.pack (show progress <> "/" <> show max_progress)
    font <- asks (.opensans12)
    labelSurf <- Font.solid font (V4 60 127 240 255) label
    labelTex <- createTextureFromSurface rdr labelSurf
    labelSize <- surfaceDimensions labelSurf
    copy rdr labelTex Nothing (Just $ Rectangle basePoint labelSize)
    pure ()

drawDamage :: AppM ()
drawDamage = do
  world <- liftIO . readIORef =<< asks (.world)
  rdr <- asks (.renderer)
  let label = "DAMAGE: " <> Text.pack (show world.damage)
            <> "  DPS: " <> Text.pack (show $ fromIntegral world.damage / fromIntegral world.tick)
            <> "  TICK: " <> Text.pack (show world.tick)
      color = V4 80 80 0 255
  font <- asks (.opensans12)
  labelSurf <- Font.solid font color label
  labelTex <- createTextureFromSurface rdr labelSurf
  labelSize <- surfaceDimensions labelSurf
  let
      baseX = 320 - labelSize^._x `div` 2
      basePoint = P (V2 baseX 20)
  copy rdr labelTex Nothing (Just $ Rectangle basePoint labelSize)
