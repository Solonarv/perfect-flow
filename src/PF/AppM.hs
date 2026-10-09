module PF.AppM where

import SDL qualified
import Control.Monad.Reader

data AppCtx = AppCtx
  { renderer :: SDL.Renderer
  , window :: SDL.Window
  -- add ?apecs? world later
  }

newtype AppM a = AppM { unAppM :: AppCtx -> IO a }
  deriving (Functor, Applicative, Monad, MonadReader AppCtx, MonadIO)
    via (ReaderT AppCtx IO)

runAppWith :: SDL.Window -> SDL.Renderer -> AppM a -> IO a
runAppWith window renderer act = act.unAppM AppCtx{..}