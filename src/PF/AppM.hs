module PF.AppM where

import SDL qualified
import Control.Monad.Reader

import PF.Simulation
import Data.IORef
import qualified SDL.Font as Font

data AppCtx = AppCtx
  { renderer :: SDL.Renderer
  , window :: SDL.Window
  , opensans12 :: Font.Font
  , world :: IORef World
  }

newtype AppM a = AppM { unAppM :: AppCtx -> IO a }
  deriving (Functor, Applicative, Monad, MonadReader AppCtx, MonadIO)
    via (ReaderT AppCtx IO)

runAppWith :: SDL.Window -> SDL.Renderer -> World -> Font.Font -> AppM a -> IO a
runAppWith window renderer initWorld opensans12 act = do
  world <- newIORef initWorld
  act.unAppM AppCtx{..}