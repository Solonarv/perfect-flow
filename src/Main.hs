{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow         ((&&&))
import           Control.Concurrent    (threadDelay)
import           Control.Monad
import           Data.Foldable
import           Data.IORef
import           Data.Monoid
import           Data.Proxy
import           Data.Traversable
import           System.Environment
-- import System.IO (hFlush, stdout)

import qualified Data.Text             as Text

import           Apecs
import           SDL                   hiding (get)
import qualified SDL.Font              as Font

import           Apecs.EntityIndex
import           Game.Engine.Input
import           Game.Engine.Settings
import           Game.Flow.LevelParser
import           Game.Flow.Resources
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
  level    <- getArgs >>= loadLevel . head
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
    rendererDrawColor r $= V4 255 255 255 0
    fillRect r Nothing
  renderResources = do
    countRef <- liftIO $ newIORef 0
    cimapM_ $ \(res, (ResAmount amt, ResBounds lo hi)) -> do
      ix <- liftIO $ readIORef countRef <* modifyIORef' countRef (+ 1)
      let y       = 10 + ix * 50
          topleft = P (V2 10 y)
      getSafe <$> get (cast res @Name) >>= \case
        Nothing        -> pure ()
        Just (Name nm) -> do
          tex <- Font.solid font (V4 0 0 0 0) nm >>= createTextureFromSurface r
          texSize <-
            uncurry V2 . (textureWidth &&& textureHeight) <$> queryTexture tex
          copy r tex Nothing (Just $ Rectangle (P (V2 320 y)) texSize)
      liftIO $ renderBar (Rectangle topleft (V2 300 50)) (amt - lo) (hi - lo)
  renderCasting =
    cmapM_ $ \(Castable casttime _cost direction, Casting progress) -> do
      let cur = case direction of
            NormalCast    -> progress
            ChanneledCast -> casttime - progress
      liftIO $ renderBar (Rectangle (P (V2 300 400)) (V2 300 50)) cur casttime
  renderBar bbox@(Rectangle pt (V2 w h)) cur full = do
    let progress = if cur == full then 1 else cur / full
    rendererDrawColor r $= V4 0 0 0 0
    drawRect r (Just bbox)
    rendererDrawColor r $= V4 130 0 0 20
    fillRect r . Just $ Rectangle
      (pt + pure 1)
      (V2 (round $ (fromIntegral $ w - 2) * progress) (h - 2))
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

