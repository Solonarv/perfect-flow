module Graphics.GL.Primitives2D where

import Data.Foldable
import Foreign

import Linear.V2
import Linear.V4
import Graphics.GL.Core43
import Graphics.GL.Types

import ForeignUtils
import Misc.AABB

data Rectangle = Rectangle
  { rectCoords :: V4 (V2 GLint) -> IO ()
  , rectColors :: V4 (V4 GLubyte) -> IO ()
  , rectDraw :: IO ()
  }

newRectangle :: IO Rectangle
newRectangle = do

  vao <- allocaPeek $ glGenVertexArrays 1
  [vboPositions, vboColors] <- allocaArrayPeek 2 $ glGenBuffers 2

  -- set up VBOs for this VAO
  glBindVertexArray vao
  
  -- bind the positions buffer
  glBindBuffer GL_ARRAY_BUFFER vboPositions
  glEnableVertexAttribArray 0
  glVertexAttribIPointer 0 2 GL_INT 0 nullPtr
  
  -- bind the colors buffer
  glBindBuffer GL_ARRAY_BUFFER vboColors
  glEnableVertexAttribArray 1
  glVertexAttribPointer 1 4 GL_UNSIGNED_BYTE GL_TRUE 0 nullPtr

  -- done binding
  glBindBuffer GL_ARRAY_BUFFER 0

  pure Rectangle
    { rectCoords = \(V4 tl tr br bl) -> do
        let fan = concatMap toList [tl, tr, br, bl]
        glBindBuffer GL_ARRAY_BUFFER vboPositions
        withArrayLen_ fan $ \len ptr ->
          glBufferData GL_ARRAY_BUFFER (fromIntegral len) ptr GL_STATIC_DRAW
        glBindBuffer GL_ARRAY_BUFFER 0
    , rectColors = \(V4 tl tr br bl) -> do  
        glBindBuffer GL_ARRAY_BUFFER vboColors
        withArrayLen_ (concatMap toList [tl, tr, br, bl]) $ \len ptr ->
          glBufferData GL_ARRAY_BUFFER (fromIntegral len) ptr GL_STATIC_DRAW
        glBindBuffer GL_ARRAY_BUFFER 0
    , rectDraw = do
        glBindVertexArray vao
        glDrawArrays GL_TRIANGLE_FAN 0 4
    }
