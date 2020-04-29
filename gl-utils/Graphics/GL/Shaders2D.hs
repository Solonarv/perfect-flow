{-# LANGUAGE TemplateHaskell #-}
module Graphics.GL.Shaders2D where

import Data.ByteString (ByteString)
import Data.FileEmbed

frag2d :: ByteString
frag2d = $(embedFile "data/assets/shaders/flat2d/frag.glsl")

vert2d :: ByteString
vert2d = $(embedFile "data/assets/shaders/flat2d/vert.glsl")