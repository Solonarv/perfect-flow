module SDL.Cache.RenderTTF where

import           Control.Monad.IO.Class
import           GHC.Generics           (Generic)

import           Data.Hashable
import           Data.Text              (Text)
import qualified SDL
import qualified SDL.Font               as SDLF

import           Data.ResourceCache
import           SDL.Cache.FontLoad
import           SDL.Monad.Renderer

-- | Which rendering method to use.
data TTFRenderMethod
  = RenderSolid -- ^ @'SDLF.solid'@ - quick and dirty. Solid text on transparent background.
  | RenderShaded !SDLF.Color  -- ^ @'SDLF.shaded'@ - slower and nicer, but with a solid-color background.
  | RenderBlended -- ^ @'SDLF.blended'@ - slowest and nicest, with a transparent background.
  deriving (Eq, Show, Generic)
instance Hashable TTFRenderMethod

data TextRenderInfo = TextRenderInfo !TTFRenderMethod !SDLF.Color !Text !FontInfo deriving (Eq, Show, Generic)
instance Hashable TextRenderInfo

renderToTexture' :: MonadIO m => (SDL.Renderer, FontCache) -> TextRenderInfo -> m SDL.Texture
renderToTexture' (rdr, fontCache) (TextRenderInfo renderMethod textColor text fontInfo) = do
  font <- getValue fontCache fontInfo
  surf <- case renderMethod of
    RenderSolid          -> SDLF.solid   font textColor         text
    RenderShaded bgColor -> SDLF.shaded  font textColor bgColor text
    RenderBlended        -> SDLF.blended font textColor         text
  tex <- SDL.createTextureFromSurface rdr surf
  SDL.freeSurface surf
  pure tex

type TextRenderCache = ResCache TextRenderInfo SDL.Texture

type MonadFontRender m = (MonadFontCache m, MonadResCache TextRenderInfo SDL.Texture m)

newTextRenderCache :: MonadIO m => FontCache -> SDL.Renderer -> m TextRenderCache
newTextRenderCache fontCache rdr = newCache renderToTexture' 8 (\_ _ tex -> SDL.destroyTexture tex) (rdr, fontCache)

newTextRenderCache_ :: (MonadFontRender m, MonadRenderer m) => m TextRenderCache
newTextRenderCache_ = do
  fontCache <- getCache
  rdr <- getRenderer
  newTextRenderCache fontCache rdr

renderText :: MonadFontRender m => TTFRenderMethod -> SDLF.Color -> FontInfo -> Text -> m SDL.Texture
renderText method color font txt = ggetValue (TextRenderInfo method color txt font)
