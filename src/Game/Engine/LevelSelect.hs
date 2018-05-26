{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Game.Engine.LevelSelect where

import           Control.Exception      (throwIO)
import           Data.Foldable
import           Data.Semigroup

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
import           Debug.Trace.StdOut
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

newtype SelectedLevel = SelectedLevel Int deriving Show
instance Monoid SelectedLevel where mempty = SelectedLevel $ -1; mappend = const id
instance Component SelectedLevel where type Storage SelectedLevel = Global SelectedLevel

newtype LevelEntry = LevelEntry Int deriving Show
instance Component LevelEntry where type Storage LevelEntry = Map LevelEntry

newtype LevelList = LevelList (Vector LevelInfo) deriving (Monoid, FromJSON)
instance Component LevelList where type Storage LevelList = Global LevelList

makeWorldWithUI "LevelSelectDialog" [''LevelList, ''SelectedLevel, ''LevelEntry]

getSelectedLevel
  :: MonadIO m
  => FilePath
  -> SDL.Window
  -> Rect
  -> SDL.Font
  -> m (Maybe FilePath)
getSelectedLevel lvlListPath win rect font = liftIO $ do
  levelList <- decodeFileEither @LevelList lvlListPath >>= either throwIO pure
  world <- initLevelSelectDialog
  liftIO $ runWith world $ do
    setGlobal $ RenderTarget win
    setGlobal levelList
    setFallback $ TxtFont font
    setFallback defaultLvlColor
    setupUI rect
    uiLoop
    SelectedLevel i <- getGlobal
    liftIO . putStrLn $ "Selected level: " <> show i
    let LevelList levels = levelList
    return $ _liPath <$> levels !? i

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

setupUI :: Rect -> System LevelSelectDialog ()
setupUI rect = do
  let LSBoxes { boxExit, boxOk, boxLevelList} = calculateBoxes rect
  world <- System ask
  newEntity (Box boxExit, Label "Exit", onLeftClick world $ setGlobal (SelectedLevel (-1)) >> setExit True)
  newEntity (Box boxOk, Label "Play", onLeftClick world $ setExit True)
  LevelList levels <- getGlobal
  let Rect lvl0 lvlDims = boxLevelList
  for_ (Vector.zip [0..] levels) $ \(i, level) -> newEntity
    ( Box $ Rect (lvl0 & _y +~ (20 * fromIntegral i)) (lvlDims & _y .~ 20)
    , Label (_liName level)
    , LevelEntry i
    , defaultLvlColor
    , onLeftClick world $ do
      Apecs.rmap $ \(LevelEntry i') -> if i == i' then highlightLvlColor else defaultLvlColor
      setGlobal =<< printId (SelectedLevel i)
    )

highlightLvlColor :: Colored
highlightLvlColor = Colored { colorFG = black, colorBG = gray 0.8 }

defaultLvlColor :: Colored
defaultLvlColor = Colored { colorFG = black, colorBG = gray 0.5}

