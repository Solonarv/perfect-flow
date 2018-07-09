{-# LANGUAGE OverloadedStrings #-}
module Game.Flow.Init where

import           Control.Monad.Reader
import qualified SDL
import qualified SDL.Font             as SDLF
import           UnliftIO

import           Control.Logging
import           Game.Engine.Settings
import           Game.Flow.Monad
import           Paths
import           SDL.Cache.FontLoad
import           SDL.Cache.RenderTTF
import           SDLUI
import           World

runGame :: Game a -> IO a
runGame game = do
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
  timeCache <- defaultTimeCache
  withLoggingEnv (LogFileNoRotate logPath defaultBufSize) timeCache Info $ \logEnv ->
    runReaderT (runGameM game) GameEnv
      { genvUIWorld       = uiWorld
      , genvGameplayWorld = world
      , genvRenderer      = renderer
      , genvFontLoader    = fontLoader
      , genvTextRender    = textRenderCache
      , genvSettings      = settingsRef
      , genvLoggingEnv    = logEnv
      }
