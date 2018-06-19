module SDLUI.Interact where

import           Control.Arrow
import           Control.Monad
import           Data.Semigroup

import           Apecs
import qualified SDL

import           Apecs.Extra
import           SDLUI.Components

handleEvent :: HasAll w [Box, Clickable, ShouldExit] => SDL.Event -> System w ()
handleEvent = SDL.eventPayload >>> \case
  SDL.WindowClosedEvent _ -> setExit True
  SDL.MouseButtonEvent evtData -> cmapM_ $ \(Box bounds, Clickable handler) ->
    let SDL.P pos = fromIntegral <$> SDL.mouseButtonEventPos evtData
    in when (pos `inRect` bounds) $ liftIO $ do putStrLn $ show pos <> "`inRect`" <> show bounds; handler evtData
  _ -> pure ()

onLeftClick :: w -> System w () ->  Clickable
onLeftClick world act = Clickable $ \evtData ->
  when (SDL.mouseButtonEventMotion evtData == SDL.Pressed && SDL.mouseButtonEventButton evtData == SDL.ButtonLeft) (runWith world act)
