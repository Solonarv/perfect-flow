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
import           Apecs.Monad
import           Game.Engine.Input
import           Game.Engine.Input.SkillIndex
import           Game.Engine.LevelSelect
import           Game.Engine.Settings
import           Game.Flow.Init
import           Game.Flow.LevelParser
import           Game.Flow.Monad
import           Game.Flow.Resources
import           Paths
import           SDL.Extra
import           SDL.Triangle
import           World

main :: IO ()
main = runGame $ do
  level    <- liftIO getArgs >>= \case
    lvl:_ -> loadLevel lvl
    [] -> getSelectedLevel (dataDir </> "levels.yaml") defaultText (Rectangle  (P 0) (V2 800 600)) >>= \case
      Nothing -> error "could not load level"
      Just lv -> loadLevel (dataDir </> lv)
  performSetup level
  mainLoop

mainLoop :: Game ()
mainLoop = do
  settings   <- getSettings
  events     <- pollEvents
  shouldExit <- for (eventPayload <$> events) $ \case
    KeyboardEvent kbEvt -> if keyboardEventKeyMotion kbEvt == Pressed
      then case lookupKeyAction (keyboardEventKeysym kbEvt) (gameSettingsKeyMap settings) of
        Nothing  -> pure False
        Just act -> liftSystem @World $ case act of
          ExitGame       -> pure True
          CancelCasting  -> False <$ resetStore (Proxy @Casting)
          Cast skillIndex -> do lookupEntity skillIndex >>= traverse_ (tryStartCasting settings . cast); pure False
      else pure False
    WindowClosedEvent _ -> pure True
    QuitEvent           -> pure True
    _                   -> pure False
  if or shouldExit
    then pure ()
    else do
      tick (1 / 30)
      render
      fixFrameTime (1 / 30)
      mainLoop

fixFrameTime :: Double -> Game ()
fixFrameTime desiredFrameTime = liftSystem @World $ do
  lastFrame <- getSum . getTime <$> getGlobal
  now       <- time
  let elapsed   = now - lastFrame
      remaining = desiredFrameTime - elapsed
  setGlobal . Time . Sum $ now
  liftIO $ when (remaining > 0) $ threadDelay (round $ remaining * 1e6)

defaultText :: FontInfo
defaultText = FontInfo defaultFont 12 0

render :: Game ()
render = do
  clearM
  renderBackdrop
  renderResources
  renderCasting
  renderDamageCounter
  presentM
 where
  renderBackdrop :: Game ()
  renderBackdrop = do
    rdrDrawColor $ setV $ V4 255 255 255 255
    fillRectM Nothing
  renderResources :: Game ()
  renderResources = do
    barsRef <- liftIO $ newIORef 0
    iconsRef <- liftIO $ newIORef 0
    lcimapM_ @World $ \(res, (ResAmount amt, ResBounds lo hi)) -> do
      idx <- liftSystem @World $ getSafe <$> get (cast res @SkillIndex)
      when (isNothing idx) $ do
        ix <- liftIO $ readIORef barsRef <* modifyIORef' barsRef (+ 1)
        let y       = 10 + ix * 50
            topleft = P (V2 10 y)
        liftSystem @World (getSafe <$> get (cast res @Name)) >>= \case
          Nothing        -> pure ()
          Just (Name nm) -> do
            tex <- renderText RenderBlended 0 defaultText nm
            texSize <- queryTextureDims tex
            copyM tex Nothing (Just $ Rectangle (P (V2 320 y)) texSize)
        renderBar (Rectangle topleft (V2 300 50)) (amt - lo) (hi - lo)
    lcimapM_ @World $ \(res, SkillIndex txt) -> do
      ix <- liftIO $ readIORef iconsRef <* modifyIORef' iconsRef (+ 1)
      let x = 240 + ix * 80
          topleft = V2 x 480
          dims = V2 80 80
      amt             <- liftSystem @World $ maybe     0 getResAmount  . getSafe <$> get (cast res @ResAmount)
      ResBounds lo hi <- liftSystem @World $ fromMaybe (ResBounds 0 0) . getSafe <$> get (cast res @ResBounds)
      costs           <- liftSystem @World $ maybe     [] castCost     . getSafe <$> get (cast res @Castable)
      let selfCostSpec = maybe (Fixed 0) snd $ listToMaybe $ filter ((==Self) . fst) costs
      selfCost <- liftSystem @World $ resolveResourceCost (cast res) selfCostSpec
      renderCooldownOverlay (Rectangle (P topleft) dims) (amt - lo) (selfCost - lo)
      liftSystem @World (getSafe <$> get (cast res @Name)) >>= \case
        Nothing        -> pure ()
        Just (Name nm) -> do
          tex <- renderText RenderBlended 0 defaultText nm
          texSize <- queryTextureDims tex
          copyM tex Nothing (Just $ Rectangle (P $ topleft - V2 0 20) texSize)
      tex <- renderText RenderBlended 0 defaultText txt
      texSize <- queryTextureDims tex
      copyM tex Nothing (Just $ Rectangle (P $ topleft + V2 0 80) texSize)
  renderCasting :: Game ()
  renderCasting =
    lcmapM_ @World $ \(Castable casttime _cost direction, Casting progress) -> do
      let cur = case direction of
            NormalCast    -> progress
            ChanneledCast -> casttime - progress
      renderBar (Rectangle (P (V2 300 400)) (V2 300 50)) cur casttime
  renderBar :: Rectangle CInt -> Double -> Double -> Game ()
  renderBar bbox@(Rectangle pt (V2 w h)) cur full = do
    let progress = if cur >= full then 1 else cur / full
    rdrDrawColor $ setV black
    drawRectM (Just bbox)
    rdrDrawColor $ setV $ V4 130 0 0 20
    fillRectM . Just $ Rectangle
      (pt + pure 1)
      (V2 (round $ (fromIntegral $ w - 2) * progress) (h - 2))
  renderCooldownOverlay :: Rectangle CInt -> Double -> Double -> Game ()
  renderCooldownOverlay bbox@(Rectangle (P base) (V2 w h)) cur full = do
    let missing = if cur >= full then 0 else 1 - cur / full
    rdrDrawColor $ setV $ V4 0 0 0 255
    drawRectM (Just bbox)
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
      rdrDrawColor $ setV $ V4 130 0 0 20
      triangleFan center ptsToDrawS
  renderDamageCounter :: Game ()
  renderDamageCounter = do
    DamageDealt (Sum dmg) <- liftSystem @World getGlobal
    let str = "Damage dealt: " <> Text.pack (show dmg)
    tex     <- renderText RenderBlended black defaultText str
    texSize <- queryTextureDims tex
    copyM tex Nothing (Just $ Rectangle (P (V2 600 100)) texSize)

performSetup :: Level -> Game ()
performSetup level = liftSystem @World $ do
  setGlobal (DamageDealt 0)
  instantiateLevel level

tick :: Double -> Game ()
tick dT = liftSystem @World $ do
  regenResources dT
  clampResources
  advanceCasting dT
  resolveCasting

