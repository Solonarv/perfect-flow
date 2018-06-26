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
loadFont' (FontInfo fp size ix) = SDLF.loadIndex fp size ix

type FontCache = ResCache FontInfo SDLF.Font

fontCacheMaxAge :: Word8
fontCacheMaxAge = 128

newFontCache :: MonadIO m => m FontCache
newFontCache = newCache_ loadFont' fontCacheMaxAge (const SDLF.free)

type MonadFontCache = MonadResCache FontInfo SDLF.Font

getFont :: MonadFontCache m => FilePath -> SDLF.PointSize -> m SDLF.Font
getFont fp ps = getFontIx fp ps 0

getFontIx ::MonadFontCache m => FilePath -> SDLF.PointSize -> SDLF.Index -> m SDLF.Font
getFontIx fp ps ix = ggetValue (FontInfo fp ps ix)
