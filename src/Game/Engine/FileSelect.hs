{-# LANGUAGE TemplateHaskell #-}
module Game.Engine.FileSelect where

import           Data.IORef
import           Foreign.C.Types        (CInt)

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Vector            (Vector)
import qualified Data.Vector            as Vector
import           Linear.V2
import           System.Directory

import qualified SDL
import qualified SDL.Font               as SDL

type Rect = SDL.Rectangle CInt
pattern Rect v0 v1 = SDL.Rectangle (SDL.P v0) v1

data FSSBoxes = FSSBoxes
  { _boxTextInput          :: Rect
  , _boxOk                 :: Rect
  , _boxFileList           :: Rect
  , _boxFileListMaxEntries :: Int
  }
makeLenses ''FSSBoxes

data FileSelectState = FSS
  { _fssCurrentDir         :: FilePath
  , _fssCurrentDirContents :: Maybe (Vector FilePath)
  , _fssSelectedFile       :: Int
  , _fssScrollOffset       :: Int
  , _fssUserInput          :: Text
  , _fssRenderer           :: SDL.Renderer
  , _fssFont               :: SDL.Font
  , _fssBoxes              :: FSSBoxes
  }
makeLenses ''FileSelectState

getSelectedFile
  :: MonadIO m
  => Maybe FilePath
  -> SDL.Renderer
  -> Rect
  -> SDL.Font
  -> m (Maybe FilePath)
getSelectedFile initialDir renderer rect font = do
  dir <- maybe (liftIO getCurrentDirectory) pure initialDir
  let initialState = FSS
        { _fssCurrentDir         = dir
        , _fssCurrentDirContents = Nothing
        , _fssSelectedFile       = 0
        , _fssScrollOffset = 0
        , _fssUserInput          = Text.pack dir
        , _fssRenderer           = renderer
        , _fssFont               = font
        , _fssBoxes = calculateBoxes rect
        }
  liftIO $ newIORef initialState >>= runSelectFile

calculateBoxes :: Rect -> FSSBoxes
calculateBoxes (Rect p0 dims) =
  let
    okDims = V2 50 20
    okOrigin = inputOrigin & _x +~ inputDims^._x
    inputDims = V2 (dims^._x - okDims^._x) 20
    inputOrigin = p0 & _y +~ selectorDims^._y
    selectorDims = dims & _y -~ 20
    selectorOrigin = p0
    lineCount = selectorDims^._y `quot` 20
  in FSSBoxes
    { _boxTextInput = Rect inputOrigin inputDims
    , _boxOk = Rect okOrigin okDims
    , _boxFileList = Rect selectorOrigin selectorDims
    , _boxFileListMaxEntries = fromIntegral lineCount
    }

runSelectFile :: IORef FileSelectState -> IO (Maybe FilePath)
runSelectFile state = do
  renderSelectFile state
  error "TODO"

renderSelectFile :: IORef FileSelectState -> IO ()
renderSelectFile state = error "TODO"
