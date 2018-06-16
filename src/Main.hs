{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Arrow                ((&&&))
import           Control.Concurrent           (threadDelay)
import           Control.Monad
import           Data.Foldable
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Traversable
import           System.Environment
import           System.FilePath
-- import System.IO (hFlush, stdout)

import qualified Data.Text                    as Text

import           Apecs
import           SDL                          hiding (get)
import qualified SDL.Font                     as Font

import           Apecs.EntityIndex
import           Game.Engine.Input
import           Game.Engine.Input.SkillIndex
import           Game.Engine.LevelSelect
import           Game.Engine.Settings
import           Game.Flow.LevelParser
import           Game.Flow.Resources
import           Paths
import           SDL.Triangle
import           World

main :: IO ()
main = do
  putStrLn "startup"
  initializeAll
  Font.initialize
  arial12  <- Font.load "c:\\windows\\fonts\\Arial.ttf" 12
  world    <- initWorld
  window   <- createWindow "Perfect Flow" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  settings <- loadGameSettings defaultGameSettingsPath
  level    <- getArgs >>= \case
    lvl:_ -> loadLevel lvl
    [] -> getSelectedLevel (dataDir </> "levels.yaml") window (Rectangle  (P 0) (V2 800 600)) arial12 >>= \case
      Nothing -> error "could not load level"
      Just lv -> loadLevel (dataDir </> lv)
  runWith world $ performSetup level
  runWith world
    $ mainLoop renderer arial12 settings
  runWith world
    $   getGlobal
    >>= liftIO
    .   putStrLn
    .   ("Total damage dealt: " ++)
    .   show
    .   getSum
    .   getDamageDealt

mainLoop :: Renderer -> Font.Font -> GameSettings -> System' ()
mainLoop renderer font settings = do
  events     <- liftIO pollEvents
  shouldExit <- for (eventPayload <$> events) $ \case
    KeyboardEvent kbEvt -> if keyboardEventKeyMotion kbEvt == Pressed
      then case lookupKeyAction (keyboardEventKeysym kbEvt) (gameSettingsKeyMap settings) of
        Nothing  -> pure False
        Just act -> case act of
          ExitGame       -> pure True
          CancelCasting  -> False <$ resetStore (Proxy @Casting)
          Cast skillIndex -> do lookupEntity skillIndex >>= traverse_ (tryStartCasting settings . cast); pure False
      else pure False
    WindowClosedEvent _ -> pure True
    QuitEvent           -> pure True
    _                   -> pure False
  if or shouldExit
    then pure () -- liftIO $ hFlush stdout
    else do
      tick (1 / 30)
      render renderer font
      -- liftIO $ hFlush stdout
      fixFrameTime (1 / 30)
      mainLoop renderer font settings

fixFrameTime :: Double -> System' ()
fixFrameTime desiredFrameTime = do
  lastFrame <- getSum . getTime <$> getGlobal
  now       <- time
  let elapsed   = now - lastFrame
      remaining = desiredFrameTime - elapsed
  setGlobal . Time . Sum $ now
  liftIO $ when (remaining > 0) $ threadDelay (round $ remaining * 1e6)



render :: Renderer -> Font.Font -> System' ()
render r font = do
  clear r
  renderBackdrop
  renderResources
  renderCasting
  renderDamageCounter
  present r
 where
  renderBackdrop = do
    rendererDrawColor r $= V4 255 255 255 255
    fillRect r Nothing
  renderResources = do
    barsRef <- liftIO $ newIORef 0
    iconsRef <- liftIO $ newIORef 0
    cimapM_ $ \(res, (ResAmount amt, ResBounds lo hi)) -> do
      idx <- getSafe <$> get (cast res @SkillIndex)
      when (isNothing idx) $ do
        ix <- liftIO $ readIORef barsRef <* modifyIORef' barsRef (+ 1)
        let y       = 10 + ix * 50
            topleft = P (V2 10 y)
        getSafe <$> get (cast res @Name) >>= \case
          Nothing        -> pure ()
          Just (Name nm) -> do
            tex <- Font.solid font (V4 0 0 0 0) nm >>= createTextureFromSurface r
            texSize <-
              uncurry V2 . (textureWidth &&& textureHeight) <$> queryTexture tex
            copy r tex Nothing (Just $ Rectangle (P (V2 320 y)) texSize)
        renderBar (Rectangle topleft (V2 300 50)) (amt - lo) (hi - lo)
    cimapM_ $ \(res, SkillIndex txt) -> do
      ix <- liftIO $ readIORef iconsRef <* modifyIORef' iconsRef (+ 1)
      let x = 240 + ix * 80
          topleft = V2 x 480
          dims = V2 80 80
      amt <- maybe 0 getResAmount . getSafe <$> get (cast res @ResAmount)
      ResBounds lo hi <- fromMaybe (ResBounds 0 0) . getSafe <$> get (cast res @ResBounds)
      costs <- maybe [] castCost . getSafe <$> get (cast res @Castable)
      let selfCostSpec = maybe (Fixed 0) snd $ listToMaybe $ filter ((==Self) . fst) costs
      selfCost <- resolveResourceCost (cast res) selfCostSpec
      renderCooldownOverlay (Rectangle (P topleft) dims) (amt - lo) (selfCost - lo)
      getSafe <$> get (cast res @Name) >>= \case
        Nothing        -> pure ()
        Just (Name nm) -> do
          tex <- Font.solid font (V4 0 0 0 0) nm >>= createTextureFromSurface r
          texSize <- uncurry V2 . (textureWidth &&& textureHeight) <$> queryTexture tex
          copy r tex Nothing (Just $ Rectangle (P $ topleft - V2 0 20) texSize)
      tex <- Font.solid font (V4 0 0 0 0) txt >>= createTextureFromSurface r
      texSize <- uncurry V2 . (textureWidth &&& textureHeight) <$> queryTexture tex
      copy r tex Nothing (Just $ Rectangle (P $ topleft + V2 0 80) texSize)
  renderCasting =
    cmapM_ $ \(Castable casttime _cost direction, Casting progress) -> do
      let cur = case direction of
            NormalCast    -> progress
            ChanneledCast -> casttime - progress
      renderBar (Rectangle (P (V2 300 400)) (V2 300 50)) cur casttime
  renderBar bbox@(Rectangle pt (V2 w h)) cur full = do
    let progress = if cur >= full then 1 else cur / full
    rendererDrawColor r $= V4 0 0 0 255
    drawRect r (Just bbox)
    rendererDrawColor r $= V4 130 0 0 20
    fillRect r . Just $ Rectangle
      (pt + pure 1)
      (V2 (round $ (fromIntegral $ w - 2) * progress) (h - 2))
  renderCooldownOverlay bbox@(Rectangle (P base) (V2 w h)) cur full = do
    let missing = if cur >= full then 0 else 1 - cur / full
    rendererDrawColor r $= V4 0 0 0 255
    drawRect r (Just bbox)
    when (missing > 0) $ do
      let basePoints = [V2 0.5 0, V2 0 0, V2 0 1, V2 1 1, V2 1 0]
          prevCorners = filter (<= missing) [-1/8, 1/8, 3/8, 5/8, 7/8] -- always non-empty because missing >= 0
          cornersPassed = length prevCorners
          angleFromCorner = missing - last prevCorners
          angleFromEdgeCenter = angleFromCorner - 1/8
          offsetFromCenter = tan (angleFromEdgeCenter * pi)
          lastPoint = case cornersPassed of
            1 -> V2 (0.5 - offsetFromCenter)   0
            2 -> V2 0                        (0.5 + offsetFromCenter)
            3 -> V2 (0.5 + offsetFromCenter) 1
            4 -> V2 1                        (0.5 - offsetFromCenter)
            5 -> V2 (0.5 - offsetFromCenter)   0
          ptsToDrawLocal = take cornersPassed basePoints ++ [lastPoint]
          toScreenCoords = P . fmap round . (+) (fromIntegral <$> base) . liftA2 (*) (fromIntegral <$> V2 w h)
          ptsToDrawS = toScreenCoords <$> ptsToDrawLocal
          center = toScreenCoords (V2 0.5 0.5)
      rendererDrawColor r $= V4 130 0 0 20
      triangleFan r center ptsToDrawS



  renderDamageCounter = do
    DamageDealt (Sum dmg) <- getGlobal
    let str = "Damage dealt: " <> Text.pack (show dmg)
    tex     <- Font.solid font (V4 0 0 0 0) str >>= createTextureFromSurface r
    texSize <-
      uncurry V2 . (textureWidth &&& textureHeight) <$> queryTexture tex
    copy r tex Nothing (Just $ Rectangle (P (V2 600 100)) texSize)

performSetup :: Level -> System' ()
performSetup level = do
  setGlobal (DamageDealt 0)
  instantiateLevel level

tick :: Double -> System' ()
tick dT = do
  regenResources dT
  clampResources
  advanceCasting dT
  resolveCasting

