module SDLUI.Interact where

import           Control.Arrow
import           Control.Monad
import           Data.Semigroup

import           Apecs
import qualified SDL

import           Apecs.Extra
import           Apecs.Monad
import           SDLUI.Components
import           SDLUI.Core

handleEvent :: MonadSdlUI m => SDL.Event -> m ()
handleEvent = SDL.eventPayload >>> \case
  SDL.WindowClosedEvent _ -> liftSdlUI $ setExit True
  SDL.MouseButtonEvent evtData -> lcmapM_ @UIWorld $ \(Box bounds, Clickable handler) ->
    let SDL.P pos = fromIntegral <$> SDL.mouseButtonEventPos evtData
    in when (pos `inRect` bounds) $ liftIO $ handler evtData
  _ -> pure ()

onLeftClick :: w -> System w () ->  Clickable
onLeftClick world act = Clickable $ \evtData ->
  when (SDL.mouseButtonEventMotion evtData == SDL.Pressed && SDL.mouseButtonEventButton evtData == SDL.ButtonLeft) (runWith world act)
