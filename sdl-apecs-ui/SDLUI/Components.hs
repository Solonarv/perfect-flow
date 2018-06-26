module SDLUI.Components
  ( module SDLUI.Components
  , module SDLUI.Components.Globals
  ) where

import           Foreign.C.Types          (CInt)
import           GHC.Word                 (Word8)

import           Control.Lens
import           Data.Default
import           Data.Text                (Text)
import           Linear.V2
import           Linear.V4

import           Apecs
import qualified SDL

import           Apecs.Default
import           Data.Ord.Extra
import           SDL.Extra
import           SDLUI.Components.Globals

type Rect = SDL.Rectangle CInt
pattern Rect :: V2 a -> V2 a -> SDL.Rectangle a
pattern Rect v0 v1 = SDL.Rectangle (SDL.P v0) v1

infix 4 `inRect`
inRect :: V2 CInt -> Rect -> Bool
inRect pos (Rect p0 dims) = (local^._x <= dims^._x) && (local^._y <= dims^._y) && local^._x >= 0 && local ^._y >= 0
  where
    local = pos - p0

newtype Box = Box { boxBounds :: Rect } deriving (Eq, Show)
instance Component Box where
  type Storage Box = Map Box

newtype Label = Label { labelText :: Text } deriving (Eq, Show)
instance Component Label where
  type Storage Label = Map Label

newtype Clickable = Clickable { handleClick :: SDL.MouseButtonEventData -> IO () }
instance Show Clickable where
  show _ = "Clickable<SDL.MouseButtonEvent handler>"
instance Component Clickable where
  type Storage Clickable = Map Clickable

data Colored = Colored { colorFG :: !Color, colorBG :: !Color } deriving (Eq, Show)
instance Default Colored where def = Colored 255 0
instance Component Colored where
  type Storage Colored = Fallback (Map Colored)

data AlignDirection = Low | Center | High  deriving (Eq, Ord, Enum, Bounded, Show)
newtype Align = Align { aligns :: V2 AlignDirection } deriving (Eq, Show)
instance Default Align where def = Align $ V2 Low Center
instance Component Align where
  type Storage Align = DefaultMap Align

doAlign :: Integral a => AlignDirection -> a -> a -> a
doAlign dir outer inner = case dir of
  Low    -> 0
  Center -> (outer - inner) `quot` 2
  High   -> outer - inner

newtype TxtFont = TxtFont { txtFont :: FontInfo }
instance Component TxtFont where
  type Storage TxtFont = Fallback (Map TxtFont)
