{-# LANGUAGE TemplateHaskell #-}
module SDL.UI where

import           Control.Monad
import           Foreign.C.Types        (CInt)
import           GHC.Word               (Word8)

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Semigroup
import           Data.StateVar          (($=))
import           Data.Text              (Text)
import           Linear.V2
import           Linear.V4

import           Apecs
import qualified SDL
import qualified SDL.Font               as SDL

import           Apecs.Util
import           Data.Ord.Extra

type Rect = SDL.Rectangle CInt
pattern Rect :: V2 a -> V2 a -> SDL.Rectangle a
pattern Rect v0 v1 = SDL.Rectangle (SDL.P v0) v1

newtype Box = Box { _boxBounds :: Rect }
instance Component Box where type Storage Box = Map Box
makeLenses ''Box

newtype TextC = TextC { _textContent :: Text }
instance Component TextC where type Storage TextC = Map TextC
makeLenses ''TextC

newtype Clickable = Clickable { _clickHandler :: forall m. MonadIO m => SDL.MouseButtonEventData -> m () }
instance Component Clickable where type Storage Clickable = Map Clickable
makeLenses ''Clickable

newtype ShouldExit = ShouldExit { _shouldExit :: Bool }
instance Semigroup ShouldExit where
  ShouldExit l <> ShouldExit r = ShouldExit $ l || r
instance Monoid ShouldExit where
  mempty = ShouldExit False
  mappend = (<>)
instance Component ShouldExit where type Storage ShouldExit = Global ShouldExit
makeLenses ''ShouldExit

exit :: Has w ShouldExit => System w ()
exit = modifyGlobal (<> ShouldExit True)

uiLoop :: HasAll w [Box, TextC, Clickable, ShouldExit] => System w ()
uiLoop = do
  ShouldExit done <- getGlobal
  unless done $
    SDL.eventPayload <$> SDL.waitEvent >>= \case
      SDL.WindowClosedEvent _ -> pure ()
      SDL.MouseButtonEvent evtData -> do
        let SDL.P pos = fromIntegral <$> SDL.mouseButtonEventPos evtData
        cmapM_ $ \(Box bounds, Clickable handler) -> when (pos `inRect` bounds) $ handler evtData


renderText :: SDL.Renderer -> SDL.Font -> Rect -> SDL.Color -> SDL.Color -> Text -> IO ()
renderText r font bounds@(Rect pos dims) fg bg text = do
  surf <- SDL.shaded font fg bg text
  tex <- SDL.createTextureFromSurface r surf
  texInfo <- SDL.queryTexture tex
  let w = min (SDL.textureWidth texInfo) (dims ^. _x)
      h = min (SDL.textureHeight texInfo) (dims ^. _y)
      shrunkDims = V2 w h
  SDL.rendererDrawColor r $= bg
  SDL.fillRect r $ Just bounds
  SDL.copy r tex (Just $ Rect 0 shrunkDims) (Just $ Rect pos shrunkDims)

black :: SDL.Color
black = V4 0 0 0 0

gray :: Double -> V4 Word8
gray deg = V4 lit lit lit 0 where lit = round $ deg * 255

infix 4 `inRect`
inRect :: V2 CInt -> Rect -> Bool
inRect pos (Rect p0 dims) = (local^._x) `between0` (dims^._x) && (local^._y) `between0` (dims^._x)
  where
    local = pos - p0
