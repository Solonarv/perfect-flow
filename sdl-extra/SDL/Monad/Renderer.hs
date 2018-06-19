module SDL.Monad.Renderer where

import           Control.Monad.IO.Class
import           Foreign.C.Types
import           GHC.Word

import           Control.Monad.Reader
import           Data.StateVar
import           Data.Vector.Storable   as Storable (Vector)
import           SDL

-- * Core functionality

-- | A monad with a @'Renderer'@ capability.
-- This is useful to avoid needing to pass the @Renderer@ around explicitly.
class MonadIO m => MonadRenderer m where
  getRenderer :: m Renderer

instance MonadIO m => MonadRenderer (ReaderT Renderer m) where
  getRenderer = ask

-- | This combinator gives access to some property of the in-context renderer using a continuation.
withRenderer :: MonadRenderer m => (Renderer -> s) -> (s -> m a) -> m a
withRenderer sv act = getRenderer >>= act . sv

-- * Lifted rendering primitives
-- These are the rendering primitives from "SDL.Video.Renderer", lifted to work in any @'MonadRenderer'@.
-- Apart from not requiring an explicit @'Renderer'@ argument, they are equivalent to their unlifted counterparts.

clearM :: MonadRenderer m => m ()
clearM = getRenderer >>= clear

copyM :: MonadRenderer m => Texture -> Maybe (Rectangle CInt) -> Maybe (Rectangle CInt) -> m ()
copyM tex src dst = do r <- getRenderer; copy r tex src dst

copyExM :: MonadRenderer m => Texture -> Maybe (Rectangle CInt) -> Maybe (Rectangle CInt) -> CDouble -> Maybe (Point V2 CInt) -> V2 Bool -> m ()
copyExM tex src dst rot fulc flip = do r <- getRenderer; copyEx r tex src dst rot fulc flip

drawLineM :: MonadRenderer m => Point V2 CInt -> Point V2 CInt -> m ()
drawLineM p q = do r <- getRenderer; drawLine r p q

drawLinesM :: MonadRenderer m => Storable.Vector (Point V2 CInt) -> m ()
drawLinesM pts = do r <- getRenderer; drawLines r pts

drawPointM :: MonadRenderer m => Point V2 CInt -> m ()
drawPointM pt = do r <- getRenderer; drawPoint r pt

drawPointsM :: MonadRenderer m => Storable.Vector (Point V2 CInt) -> m ()
drawPointsM pts = do r <- getRenderer; drawPoints r pts

drawRectM :: MonadRenderer m => Maybe (Rectangle CInt) -> m ()
drawRectM rect = do r <- getRenderer; drawRect r rect

drawRectsM :: MonadRenderer m => Storable.Vector (Rectangle CInt) -> m ()
drawRectsM rects = do r <- getRenderer; drawRects r rects

fillRectM :: MonadRenderer m => Maybe (Rectangle CInt) -> m ()
fillRectM rect = do r <- getRenderer; fillRect r rect

fillRectsM :: MonadRenderer m => Storable.Vector (Rectangle CInt) -> m ()
fillRectsM rects = do r <- getRenderer; fillRects r rects

presentM :: MonadRenderer m => m ()
presentM = getRenderer >>= present

-- * Renderer state
-- These functions allow you to access the in-context renderer's state. They take a continuation
-- which has access to one of the renderer's state variables.
-- Pass @'Data.StateVar.get'@ to retrieve the value, or @'Data.StateVar.Extra.set' x@ to set it.

rdrDrawBlendMode :: MonadRenderer m => (StateVar BlendMode -> m a) -> m a
rdrDrawBlendMode = withRenderer rendererDrawBlendMode

rdrDrawColor :: MonadRenderer m => (StateVar (V4 Word8) -> m a) -> m a
rdrDrawColor = withRenderer rendererDrawColor

rdrRenderTarget :: MonadRenderer m => (StateVar (Maybe Texture) -> m a) -> m a
rdrRenderTarget = withRenderer rendererRenderTarget

rdrClipRect :: MonadRenderer m => (StateVar (Maybe (Rectangle CInt)) -> m a) -> m a
rdrClipRect = withRenderer rendererClipRect

rdrLogicalSize :: MonadRenderer m => (StateVar (Maybe (V2 CInt)) -> m a) -> m a
rdrLogicalSize = withRenderer rendererLogicalSize

rdrScale :: MonadRenderer m => (StateVar (V2 CFloat) -> m a) -> m a
rdrScale = withRenderer rendererScale

rdrViewport :: MonadRenderer m => (StateVar (Maybe (Rectangle CInt)) -> m a) -> m a
rdrViewport = withRenderer rendererViewport

rdrTargetSupported :: MonadRenderer m => m Bool
rdrTargetSupported = getRenderer >>= renderTargetSupported
