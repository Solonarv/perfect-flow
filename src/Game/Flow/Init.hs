{-# LANGUAGE OverloadedStrings #-}
module Game.Flow.Init where

import qualified SDL
import qualified SDL.Font             as SDLF
import           UnliftIO

import           Game.Engine.Settings
import           Game.Flow.Monad
import           SDL.Cache.FontLoad
import           SDL.Cache.RenderTTF
import           SDLUI
import           World

initGame :: IO GameEnv
initGame = do
  settings        <- loadGameSettings defaultGameSettingsPath
  settingsRef     <- newIORef settings
  SDL.initializeAll
  SDLF.initialize
  world           <- initWorld
  uiWorld         <- initUIWorld
  window          <- SDL.createWindow "Perfect Flow" SDL.defaultWindow
  renderer        <- SDL.createRenderer window (-1) SDL.defaultRenderer
  fontLoader      <- newFontCache
  textRenderCache <- newTextRenderCache fontLoader renderer
  pure GameEnv
    { genvUIWorld       = uiWorld
    , genvGameplayWorld = world
    , genvRenderer      = renderer
    , genvFontLoader    = fontLoader
    , genvTextRender    = textRenderCache
    , genvSettings      = settingsRef
    }
