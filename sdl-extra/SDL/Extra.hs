module SDL.Extra
  ( module SDL.Extra
  , CInt
  , module SDL.Cache.FontLoad
  , module SDL.Cache.RenderTTF
  , module SDL.Monad.Renderer
  , module SDL.Color
  , module SDL.Triangle
  ) where

import           Control.Monad.IO.Class
import           Foreign.C.Types        (CInt)

import           Linear.V2
import qualified SDL

import           SDL.Cache.FontLoad
import           SDL.Cache.RenderTTF
import           SDL.Color
import           SDL.Monad.Renderer
import           SDL.Triangle

queryTextureDims :: MonadIO m => SDL.Texture -> m (V2 CInt)
queryTextureDims tex = do info <- SDL.queryTexture tex; pure $ V2 (SDL.textureWidth info) (SDL.textureHeight info)
