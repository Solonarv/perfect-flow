{-# LANGUAGE BlockArguments #-}
module Graphics.GL.Shaders where

import Control.Exception
import Control.Monad.IO.Class
import Data.Foldable
import Foreign

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackMallocCStringLen)
import Graphics.GL.Core40
import Graphics.GL.Types

import ForeignUtils

data ShaderSource = ShaderFile FilePath | ShaderCode ByteString
  deriving (Show, Eq)

loadShaderSource :: MonadIO m => ShaderSource -> m ByteString
loadShaderSource (ShaderFile path) = liftIO $ ByteString.readFile path
loadShaderSource (ShaderCode code) = pure code

data ShaderSpec = ShaderSpec
  { shaderspecVertex :: ShaderSource
  , shaderspecFragment :: ShaderSource
  , shaderspecAttribLocations :: [(GLuint, ByteString)]
  }
  deriving (Show, Eq)

newtype ShaderProgram = ShaderProgram { shaderProgramName :: GLuint }

useShaderProgram :: MonadIO m => ShaderProgram -> m ()
useShaderProgram (ShaderProgram program) = glUseProgram program

data GLShaderProgramException
  = GLCompileShaderException ByteString
  | GLLinkProgramException ByteString
  deriving (Eq, Show)

instance Exception GLShaderProgramException

loadShaderPipeline :: MonadIO m => ShaderSpec -> m ShaderProgram
loadShaderPipeline (ShaderSpec vertSource fragSource attribs) = liftIO do
  -- load code
  vertCode <- loadShaderSource vertSource
  fragCode <- loadShaderSource fragSource

  -- compile vertex shader
  vertShader <- compileShaderFromBS GL_VERTEX_SHADER vertCode
  fragShader <- compileShaderFromBS GL_FRAGMENT_SHADER fragCode

  -- create program, attach shaders
  program <- glCreateProgram
  glAttachShader program vertShader
  glAttachShader program fragShader

  -- attribute locations
  for_ attribs \(index, name) ->
    ByteString.useAsCString name \str ->
      glBindAttribLocation program index str
  
  -- link and check for errors
  glLinkProgram program
  traverse_ (throwIO . GLLinkProgramException) =<< getProgramError program

  pure (ShaderProgram program)

compileShaderFromBS :: GLenum -> ByteString -> IO GLuint
compileShaderFromBS shaderKind code =
  bracketOnError (glCreateShader shaderKind) glDeleteShader \shader -> do
    shaderSourceBS shader code
    glCompileShader shader
    traverse_ (throwIO . GLCompileShaderException) =<< getShaderError shader
    pure shader

shaderSourceBS :: GLuint -> ByteString -> IO ()
shaderSourceBS shader code =
  unsafeUseAsCStringLen code \(str, len) ->
      Foreign.with str \p_str ->
        Foreign.with (fromIntegral len) \p_len ->
          glShaderSource shader 1 p_str p_len

getShaderError :: GLuint -> IO (Maybe ByteString)
getShaderError shader = do
  isCompiled <- allocaPeek $ glGetShaderiv shader GL_COMPILE_STATUS
  if isCompiled /= GL_FALSE
    then pure Nothing
    else Just <$> do
      len <- allocaPeek $ glGetShaderiv shader GL_INFO_LOG_LENGTH
      bracketOnError (mallocBytes $ fromIntegral len) free \buf -> do
        actualLen <- allocaPeek \p_len ->
          glGetShaderInfoLog shader len p_len buf
        unsafePackMallocCStringLen (buf, fromIntegral actualLen)

getProgramError :: GLuint -> IO (Maybe ByteString)
getProgramError program = do
  isCompiled <- allocaPeek $ glGetProgramiv program GL_LINK_STATUS
  if isCompiled /= GL_FALSE
    then pure Nothing
    else Just <$> do
      len <- allocaPeek $ glGetProgramiv program GL_INFO_LOG_LENGTH
      bracketOnError (mallocBytes $ fromIntegral len) free \buf -> do
        actualLen <- allocaPeek \p_len ->
          glGetProgramInfoLog program len p_len buf
        unsafePackMallocCStringLen (buf, fromIntegral actualLen)