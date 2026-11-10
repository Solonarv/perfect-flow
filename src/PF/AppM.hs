{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module PF.AppM where

import SDL qualified
import Control.Monad.Reader

import PF.Simulation
import Data.IORef
import qualified SDL.Font as Font
import PF.Data.HierarchicalTable (HTable)
import PF.Locale (Locale)
import GHC.Records
import GHC.OverloadedLabels (IsLabel (..))
import Control.Monad (join)

data AppCtx = AppCtx
  { renderer :: SDL.Renderer
  , window :: SDL.Window
  , opensans12 :: Font.Font
  , world :: IORef World
  , locale :: IORef Locale
  }

newtype AppM a = AppM { unAppM :: AppCtx -> IO a }
  deriving (Functor, Applicative, Monad, MonadReader AppCtx, MonadIO)
    via (ReaderT AppCtx IO)

runAppWith :: SDL.Window -> SDL.Renderer -> World -> Locale -> Font.Font -> AppM a -> IO a
runAppWith window renderer initWorld loc opensans12 act = do
  world <- newIORef initWorld
  locale <- newIORef loc
  act.unAppM AppCtx{..}

instance HasField "value" (IORef a) (AppM a) where
  getField = liftIO . readIORef

askM :: (MonadReader e m, MonadIO m) => (e -> m r) -> m r
askM = join . asks