{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.Monoid  
import Data.Traversable
import System.Environment
import System.IO (hFlush, stdout)

import Apecs
import SDL hiding (get)
import qualified SDL.Font as Font

import qualified Data.Text.IO as Text

import Apecs.EntityIndex
import Game.Flow.LevelParser
import Game.Flow.Resources
import World

main :: IO ()
main = do
  putStrLn "startup"
  initializeAll
  Font.initialize
  arial12  <- Font.load "c:\\windows\\fonts\\Arial.ttf" 12
  world    <- initWorld
  window   <- createWindow "Perfect Flow" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  runSystem initializeEntities world
  runWith world $ mainLoop renderer arial12
  runWith world
    $   getGlobal
    >>= liftIO
    .   putStrLn
    .   ("Total damage dealt: " ++)
    .   show
    .   getSum
    .   getDamageDealt

mainLoop :: Renderer -> Font.Font -> System' ()
mainLoop renderer font = do
  events     <- liftIO pollEvents
  shouldExit <- for (eventPayload <$> events) $ \case
    KeyboardEvent kbEvt -> if keyboardEventKeyMotion kbEvt == Pressed
      then
        let key = keysymKeycode (keyboardEventKeysym kbEvt)
        in  if key == KeycodeF4
              then pure True
              else False <$ do
                when (key == KeycodeSpace) $ tryStartCasting $ Name "strike"
      else pure False
    WindowClosedEvent _ -> pure True
    QuitEvent           -> pure True
    _                   -> pure False
  if or shouldExit
    then liftIO $ hFlush stdout
    else do
      tick 1
      render renderer font
      liftIO $ hFlush stdout
      fixFrameTime (1 / 30)
      mainLoop renderer font

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
  -- renderDamageCounter
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
      -- TODO render name of resource
      getSafe <$> get (cast res @Name) >>= \case
        Nothing        -> pure ()
        Just (Name nm) -> do
          tex <- Font.solid font (V4 0 0 0 0) nm >>= createTextureFromSurface r
          texSize <-
            uncurry V2 . (textureWidth &&& textureHeight) <$> queryTexture tex
          copy r tex Nothing (Just $ Rectangle (P (V2 320 y)) texSize)
      liftIO
        $ renderBar (Rectangle topleft (V2 300 50))
        $ (amt - lo)
        / (hi - lo)
  renderCasting =
    cmapM_ $ \(Castable casttime _cost direction, Casting progress) -> do
      let progressRaw  = progress / casttime
          barFillLevel = case direction of
            NormalCast    -> progressRaw
            ChanneledCast -> 1 - progressRaw
      liftIO $ renderBar (Rectangle (P (V2 300 400)) (V2 300 50)) barFillLevel
  renderBar bbox@(Rectangle pt (V2 w h)) progress = do
    rendererDrawColor r $= V4 0 0 0 0
    drawRect r (Just bbox)
    rendererDrawColor r $= V4 130 0 0 20
    fillRect r . Just $ Rectangle
      (pt + pure 1)
      (V2 (round $ (fromIntegral $ w - 2) * progress) (h - 2))

initializeEntities :: System' ()
initializeEntities = do
  setGlobal (DamageDealt 0)
  levelPath <- head <$> liftIO getArgs
  -- newEntity (Name "Strike", (ResAmount 100, ResBounds 0 100, ResRegen 1, ResRenderAsBar), Castable 30 [(Self, Max)] NormalCast, OnCastCompleted [Damage 100])
  loadLevel levelPath
  pure ()

tick :: Double -> System' ()
tick dT = do
  regenResources
  clampResources
  advanceCasting
  resolveCasting
 where
  regenResources =
    rmap $ \(ResAmount amt, ResRegen reg) -> ResAmount (amt + reg * dT)
  clampResources =
    rmap $ \(ResAmount amt, ResBounds lo hi) -> ResAmount (clamp lo hi amt)
  advanceCasting = cmap $ \(Casting progress) -> Casting (progress + dT)
  resolveCasting =
    cimapM_ $ \(e, (Casting progress, Castable casttime _cost _direction)) ->
      when (progress >= casttime) $ do
        destroy $ cast e @Casting
        get (cast e @Name) >>= liftIO . \case
          Safe Nothing         -> putStrLn "Finished casting"
          Safe (Just (Name n)) -> Text.putStrLn $ "Finished casting " <> n
        get (cast e @OnCastCompleted) >>= \case
          Safe Nothing                       -> pure ()
          Safe (Just (OnCastCompleted acts)) -> for_ acts $ \case
            Damage dmg -> do
              liftIO . putStrLn $ "Dealt " ++ show dmg ++ " damage!"
              modifyGlobal $ mappend $ DamageDealt (Sum dmg)

clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = min hi (max x lo)
