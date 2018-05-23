{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Game.Engine.LevelSelect where

import           Control.Exception      (throwIO)
import           Control.Monad
import           Data.Foldable
import           Data.IORef
import           Foreign.C.Types        (CInt)
import           GHC.Word

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Vector            (Vector, (!?))
import qualified Data.Vector            as Vector
import           Data.Yaml.Include
import           Linear.V2
import           Linear.V4

import           SDL                    (($=))
import qualified SDL
import qualified SDL.Font               as SDL

import           Data.Ord.Extra
import           Data.StateVar.Util
import           Debug.Trace.StdOut

type Rect = SDL.Rectangle CInt
pattern Rect :: V2 a -> V2 a -> SDL.Rectangle a
pattern Rect v0 v1 = SDL.Rectangle (SDL.P v0) v1

data LevelInfo = LevelInfo
  { _liPath :: FilePath
  , _liName :: Text
  } deriving (Eq, Show)
makeLenses ''LevelInfo
instance FromJSON LevelInfo where
  parseJSON = withObject "level info" $ \o -> LevelInfo
    <$> o .: "path"
    <*> o .: "name"

data LSSBoxes = LSSBoxes
  { _boxExit                :: Rect
  , _boxOk                  :: Rect
  , _boxLevelList           :: Rect
  , _boxLevelListMaxEntries :: Int
  }
makeLenses ''LSSBoxes

data LevelSelectState = LSS
  { _lssSelectedLevel :: Int
  , _lssScrollOffset  :: Int
  , _lssLevelList     :: Vector LevelInfo
  , _lssRenderer      :: SDL.Renderer
  , _lssFont          :: SDL.Font
  , _lssBoxes         :: LSSBoxes
  }
makeLenses ''LevelSelectState

getSelectedLevel
  :: MonadIO m
  => FilePath
  -> SDL.Renderer
  -> Rect
  -> SDL.Font
  -> m (Maybe FilePath)
getSelectedLevel lvlListPath renderer rect font = do
  levelList <- liftIO $ decodeFileEither lvlListPath >>= either throwIO pure
  let initialState = LSS
        { _lssSelectedLevel      = -1
        , _lssScrollOffset       = 0
        , _lssLevelList          = levelList
        , _lssRenderer           = renderer
        , _lssFont               = font
        , _lssBoxes              = calculateBoxes rect
        }
  liftIO $ newIORef initialState >>= runSelectLevel

calculateBoxes :: Rect -> LSSBoxes
calculateBoxes (Rect p0 dims) =
  let
    buttonDims = V2 50 20
    okOrigin = p0
      & _x +~ (dims^._x - buttonDims^._x)
      & _y +~ (dims^._y - buttonDims^._y)
    exitOrigin = p0
      & _y +~ (dims^._y - buttonDims^._y)
    selectorDims = dims & _y -~ 20
    selectorOrigin = p0
    lineCount = selectorDims^._y `quot` 20
  in LSSBoxes
    { _boxExit = Rect exitOrigin buttonDims
    , _boxOk = Rect okOrigin buttonDims
    , _boxLevelList = Rect selectorOrigin selectorDims
    , _boxLevelListMaxEntries = fromIntegral lineCount
    }

runSelectLevel :: IORef LevelSelectState -> IO (Maybe FilePath)
runSelectLevel state = do
  readIORef state >>= renderSelectLevel
  SDL.eventPayload <$> SDL.waitEvent >>= \case
    SDL.WindowClosedEvent _ -> pure Nothing
    SDL.MouseButtonEvent evtData ->
      if SDL.mouseButtonEventMotion evtData == SDL.Pressed && SDL.mouseButtonEventButton evtData == SDL.ButtonLeft
        then do
          let SDL.P pos = fromIntegral <$> SDL.mouseButtonEventPos evtData
          stateNow <- readIORef state
          let boxes = stateNow ^. lssBoxes
          if pos `inRect` boxes ^. boxOk
            then pure . fmap (view liPath) $ (stateNow^.lssLevelList) !? (stateNow^.lssSelectedLevel)
            else if pos `inRect` boxes ^. boxExit
              then pure Nothing
              else do
                when (pos `inRect` boxes ^. boxLevelList) $ do
                  let
                    Rect p _ = boxes^.boxLevelList
                    clickedIx = (pos^._y - p^._y) `quot` 20
                  when (clickedIx `between` (0, fromIntegral $ length $ stateNow^.lssLevelList)) $
                    modifyIORef state $ lssSelectedLevel .~ fromIntegral clickedIx
                runSelectLevel state
        else runSelectLevel state
    _ -> runSelectLevel state

renderSelectLevel :: LevelSelectState -> IO ()
renderSelectLevel state = do
  let r = state ^. lssRenderer
      font = state ^. lssFont
      boxes = state ^. lssBoxes
      vecBoxes = [ boxes ^. boxExit
                 , boxes ^. boxOk
                 , boxes ^. boxLevelList
                 ]
  SDL.rendererDrawColor r $= black
  SDL.drawRects r vecBoxes
  SDL.rendererDrawColor r $= gray 0.2
  SDL.fillRects r vecBoxes
  renderText r font (boxes ^. boxOk)   black (gray 0.8) "OK"
  renderText r font (boxes ^. boxExit) black (gray 0.8) "Exit"
  renderLevelList state
  SDL.present r

renderLevelList :: LevelSelectState -> IO ()
renderLevelList state = do
  let entries = state^.lssLevelList
      r = state^.lssRenderer
  SDL.rendererDrawColor r $= black
  let offset = Vector.drop (state ^. lssScrollOffset) entries
      paired = Vector.zip [0..(state ^. lssBoxes . boxLevelListMaxEntries - 1)] offset
      enclosingBox = state ^. lssBoxes . boxLevelList
      Rect topleft dims = enclosingBox
      entryDims = dims & _y .~ 20
  for_ paired $ \(ix, level) -> do
    let rdrTarget = Rect (topleft & _y +~ (20 * fromIntegral ix)) entryDims
        bgcolor = if ix == state ^. lssSelectedLevel then gray 0.8 else gray 0.2
        fgcolor = black
        levelName = level ^. liName
    renderText r (state ^. lssFont) rdrTarget fgcolor bgcolor levelName

renderText :: SDL.Renderer -> SDL.Font -> Rect -> SDL.Color -> SDL.Color -> Text -> IO ()
renderText r font (Rect pos dims) fg bg text = do
  surf <- SDL.shaded font fg bg text
  tex <- SDL.createTextureFromSurface r surf
  texInfo <- SDL.queryTexture tex
  let w = min (SDL.textureWidth texInfo) (dims ^. _x)
      h = min (SDL.textureHeight texInfo) (dims ^. _y)
      shrunkDims = V2 w h
  SDL.copy r tex (Just $ Rect 0 shrunkDims) (Just $ Rect pos shrunkDims)

black :: SDL.Color
black = V4 0 0 0 0

gray :: Double -> V4 Word8
gray deg = V4 lit lit lit 0 where lit = round $ deg * 255

infix 4 `inRect`
inRect :: V2 CInt -> Rect -> Bool
inRect pos rect@(Rect p0 dims) = (local^._x `between0` dims^._x) && (local^._y `between0` dims^._x)
  where
    local = pos - p0
