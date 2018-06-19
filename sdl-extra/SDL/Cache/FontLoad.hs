{-# LANGUAGE AllowAmbiguousTypes #-}
module SDL.Cache.FontLoad where

import           Control.Monad.IO.Class
import           Data.Word
import           GHC.Generics           (Generic)

import           Data.Hashable
import qualified SDL.Font               as SDLF

import           Data.ResourceCache

data FontInfo = FontInfo !FilePath !SDLF.PointSize !SDLF.Index deriving (Eq, Show, Generic)
instance Hashable FontInfo

loadFont' :: MonadIO m => FontInfo -> m SDLF.Font
loadFont' (FontIndex fp size ix) = SDLF.loadIndex fp size ix

type FontCache = ResCache FontInfo SDLF.Font

fontCacheMaxAge :: Word8
fontCacheMaxAge = 128

newFontCache :: MonadIO m => m FontCache
newFontCache = newCache_ loadFont' fontCacheMaxAge (const SDLF.free)

getFont :: MonadResCache FontIndex SDLF.Font m => FilePath -> SDLF.PointSize -> m SDLF.Font
getFont fp ps = getFontIndex fp ps 0

getFontIndex ::MonadResCache FontIndex SDLF.Font m => FilePath -> SDLF.PointSize -> SDLF.Index -> m SDLF.Font
getFontIndex fp ps ix = ggetValue (FontInfo fp ps ix)
