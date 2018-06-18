module SDL.Color where

import           GHC.Word  (Word8)

import           Linear.V4

type Color = V4 Word8

gray :: Double -> Color
gray i = full b b b where b = round $ i * 255

black, white :: Color
black = gray 0
white = gray 1

full :: Word8 -> Word8 -> Word8 -> Color
full r g b = V4 r g b 255

transparent :: Color
transparent = 0
