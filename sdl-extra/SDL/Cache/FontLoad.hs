{-# LANGUAGE AllowAmbiguousTypes #-}
module SDL.Cache.FontLoad where

import           Control.Monad.IO.Class
import           Data.Word
import           GHC.Generics           (Generic)

import           Data.Hashable
import qualified SDL.Font               as SDLF

import           Data.ResourceCache

data FontIndex = FontIndex !FilePath !SDLF.PointSize !SDLF.Index deriving (Eq, Show, Generic)
instance Hashable FontIndex

loadFont' :: MonadIO m => FontIndex -> m SDLF.Font
loadFont' (FontIndex fp size ix) = SDLF.loadIndex fp size ix

type FontCache = ResCache FontIndex SDLF.Font

fontCacheMaxAge :: Word8
fontCacheMaxAge = 128

newFontCache :: MonadIO m => m FontCache
newFontCache = newCache loadFont' fontCacheMaxAge (const SDLF.free)

getFont :: forall sym m. MonadResCache sym FontIndex SDLF.Font m => FontIndex -> m SDLF.Font
getFont = ggetValue (Label @sym)
