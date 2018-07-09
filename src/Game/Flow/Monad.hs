module Game.Flow.Monad where

import           Control.Monad.Fix

import           Control.Monad.Reader
import           UnliftIO

import           Apecs.Monad
import           Control.Logging
import           Data.ResourceCache
import           Game.Engine.Settings
import           SDL.Cache.FontLoad
import           SDL.Cache.RenderTTF
import           SDL.Monad.Renderer
import           SDLUI
import           World

import qualified SDL
import qualified SDL.Font             as SDLF

data GameEnv = GameEnv
  { genvUIWorld       :: UIWorld
  , genvGameplayWorld :: World
  , genvRenderer      :: SDL.Renderer
  , genvFontLoader    :: FontCache
  , genvTextRender    :: TextRenderCache
  , genvSettings      :: IORef GameSettings
  , genvLoggingEnv    :: LoggingEnv
  }

newtype Game a = Game { runGameM :: ReaderT GameEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadUnliftIO, MonadReader GameEnv)

runGameWith :: GameEnv -> Game a -> IO a
runGameWith env act = runReaderT (runGameM act) env

instance MonadSystem UIWorld Game where
  getWorld = asks genvUIWorld

instance MonadSystem World Game where
  getWorld = asks genvGameplayWorld

instance MonadRenderer Game where
  getRenderer = asks genvRenderer

instance MonadResCache FontInfo SDLF.Font Game where
  getCache = asks genvFontLoader

instance MonadResCache TextRenderInfo SDL.Texture Game where
  getCache = asks genvTextRender

instance MonadSettings Game where
  getSettings = asks genvSettings >>= liftIO . readIORef

instance MonadLog Game where
  logM = defaultLogM (asks genvLoggingEnv)
