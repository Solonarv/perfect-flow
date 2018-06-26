{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Game.Engine.LevelSelect where

import           Control.Exception      (throwIO)
import           Data.Foldable
import           Data.IORef
import           Data.Semigroup
import           Data.Traversable

import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Text              (Text)
import           Data.Vector            (Vector, (!?))
import qualified Data.Vector            as Vector
import           Data.Yaml.Include
import           Linear.V2

import           Apecs
import qualified SDL
import qualified SDL.Font               as SDL

import           Apecs.Default
import           Apecs.Monad
import           Debug.Trace.StdOut
import           SDL.Cache.FontLoad
import           SDL.Color
import           SDLUI

data LevelInfo = LevelInfo
  { _liPath :: FilePath
  , _liName :: Text
  } deriving (Eq, Show)
makeLenses ''LevelInfo
instance FromJSON LevelInfo where
  parseJSON = withObject "level info" $ \o -> LevelInfo
    <$> o .: "path"
    <*> o .: "name"

data LSBoxes = LSBoxes
  { boxExit                :: Rect
  , boxOk                  :: Rect
  , boxLevelList           :: Rect
  , boxLevelListMaxEntries :: Int
  }

getSelectedLevel :: MonadSdlUI m => FilePath -> FontInfo -> Rect -> m (Maybe FilePath)
getSelectedLevel lvlListPath font rect = do
  levelList <- liftIO $ decodeFileEither lvlListPath >>= either throwIO pure
  selectedLevelRef <- liftIO $ newIORef (-1)
  cleanup <- setupUI rect levelList selectedLevelRef
  liftSdlUI $ do
    setFallback defaultLvlColor
    setFallback (TxtFont font)
  uiLoop
  lvlIndex <- liftIO $ readIORef selectedLevelRef
  liftIO . putStrLn $ "Selected level: " <> show lvlIndex
  liftIO cleanup
  return $ _liPath <$> levelList !? lvlIndex

calculateBoxes :: Rect -> LSBoxes
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
  in LSBoxes
    { boxExit = Rect exitOrigin buttonDims
    , boxOk = Rect okOrigin buttonDims
    , boxLevelList = Rect selectorOrigin selectorDims
    , boxLevelListMaxEntries = fromIntegral lineCount
    }

setupUI :: MonadSdlUI m => Rect -> Vector LevelInfo -> IORef Int -> m (IO ())
setupUI rect levels selectedLevelRef = liftSdlUI $ do
  let LSBoxes { boxExit, boxOk, boxLevelList} = calculateBoxes rect
  world <- getWorld @UIWorld
  exit <- newEntity (Box boxExit, Label "Exit", onLeftClick world $ liftIO (writeIORef selectedLevelRef (-1)) >> setExit True)
  play <- newEntity (Box boxOk, Label "Play", onLeftClick world $ setExit True)
  let Rect lvl0 lvlDims = boxLevelList
  rec
    levelEntries <- for (Vector.zip [0..] levels) $ \(self, level) -> newEntity
      ( Box $ Rect (lvl0 & _y +~ (20 * fromIntegral self)) (lvlDims & _y .~ 20)
      , Label (_liName level)
      , defaultLvlColor
      , onLeftClick world $ do
        Vector.imapM_
          (\other ety ->
            if other == self
              then set ety highlightLvlColor
              else set ety defaultLvlColor)
          levelEntries
        liftIO $ writeIORef selectedLevelRef self
      )
  pure $ runWith world $ do
    destroy exit
    destroy play
    traverse_ destroy levelEntries

highlightLvlColor :: Colored
highlightLvlColor = Colored { colorFG = black, colorBG = gray 0.8 }

defaultLvlColor :: Colored
defaultLvlColor = Colored { colorFG = black, colorBG = gray 0.5}

