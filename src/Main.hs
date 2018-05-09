{-# language OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.Monoid
import Data.Traversable
import System.IO (stdout, hFlush)

import Apecs
import qualified Apecs.Slice as Slice
import SDL hiding (get)

import qualified Data.Text.IO as Text

import World

main :: IO ()
main = do
  initializeAll
  world <- initWorld
  window <- createWindow "Perfect Flow" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  runSystem initializeEntities world
  runWith world $ mainLoop renderer
  runWith world $ getGlobal >>= liftIO . putStrLn . ("Total damage dealt: "++) . show . getSum . getDamageDealt

mainLoop :: Renderer -> System' ()
mainLoop renderer = do
  events <- liftIO pollEvents
  shouldExit <- for (eventPayload <$> events) $ \case
    KeyboardEvent kbEvt ->
      if keyboardEventKeyMotion kbEvt == Pressed
        then
          let key = keysymKeycode (keyboardEventKeysym kbEvt)
          in if key == KeycodeF4
            then pure True
            else False <$ do
              when (key == KeycodeSpace) $ tryStartCasting $ Name "Strike"
        else pure False
    WindowClosedEvent _ -> pure True
    QuitEvent -> pure True
    _ -> pure False
  if or shouldExit
    then liftIO $ hFlush stdout
    else do
      tick 1
      render renderer
      liftIO $ hFlush stdout
      fixFrameTime (1/30)
      mainLoop renderer

fixFrameTime :: Double -> System' ()
fixFrameTime desiredFrameTime = do
  lastFrame <- getSum . getTime <$> getGlobal
  now <- time
  let elapsed = now - lastFrame
      remaining = desiredFrameTime - elapsed
  setGlobal . Time . Sum $ now
  liftIO $ when (remaining > 0) $ threadDelay (round $ remaining * 1e6)

-- NOTE: getUnsafe is used only on entities that are known to have the requested components
-- as noted in their type. The one use of @getUnsafe . cast@ is safe because the @cast@ is a down-cast.
tryStartCasting :: Name -> System' ()
tryStartCasting spellName = do
  named <- owners @(Name, Castable, ResAmount) >>= Slice.filterM (fmap (==spellName) . getUnsafe . cast)
  for_ (Slice.toList named) $ \toCast -> do
    alreadyCasting <- exists @_ @Casting (cast toCast)
    when (not alreadyCasting) $ do
      (_, Castable cost _time, ResAmount energy) <- getUnsafe toCast -- safe, see NOTE
      when (cost <= energy) $ do
        modify (cast toCast) (ResAmount . subtract cost . getResAmount)
        set toCast (Casting 0)

render :: Renderer -> System' ()
render r = do
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
        ix <- liftIO $ readIORef countRef <* modifyIORef' countRef (+1)
        let topleft = P (V2 10 (10 + ix * 50))
        -- TODO render name of resource
        -- Name nm <- fromMaybe (Name "") . getSafe <$> get (cast res @Name)
        liftIO $ renderBar (Rectangle topleft (V2 300 50)) $ (amt - lo) / (hi - lo)
    renderCasting = do
      cmapM_ $ \(Castable _ casttime, Casting progress) -> do
        liftIO $ renderBar (Rectangle (P (V2 300 400)) (V2 300 50)) (progress / casttime)
    renderBar bbox@(Rectangle pt (V2 w h)) progress = do
      rendererDrawColor r $= V4 0 0 0 0
      drawRect r (Just bbox)
      rendererDrawColor r $= V4 130 0 0 20
      fillRect r . Just $ Rectangle (pt + pure 1) (V2 (round $ (fromIntegral $ w-2) * progress) (h-2))
        


initializeEntities :: System' ()
initializeEntities = do
  setGlobal (DamageDealt 0)
  newEntity (Name "Strike", ResAmount 100, ResBounds 0 100, ResRegen 1, Castable 100 30, Damage 100)
  pure ()

tick :: Double -> System' ()
tick dT = do
  regenResources
  clampResources
  advanceCasting
  resolveCasting
  where
    regenResources = rmap $ \(ResAmount amt, ResRegen reg) -> ResAmount (amt + reg * dT)
    clampResources = rmap $ \(ResAmount amt, ResBounds lo hi) -> ResAmount (clamp lo hi amt)
    advanceCasting = cmap $ \(Casting progress) -> Casting (progress + dT)
    resolveCasting = cimapM_ $ \(e, (Casting progress, Castable _cost casttime)) ->
      when (progress >= casttime) $ do
        destroy $ cast e @Casting
        get (cast e @Name) >>= liftIO . \case
          Safe Nothing -> putStrLn "Finished casting"
          Safe (Just (Name n)) -> Text.putStrLn $ "Finished casting " <> n
        get (cast e @Damage) >>= \case
          Safe Nothing -> pure ()
          Safe (Just (Damage dmg)) -> do
            liftIO . putStrLn $ "Dealt " ++ show dmg ++ " damage!"
            modifyGlobal $ mappend $ DamageDealt (Sum dmg)


clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = min hi (max x lo)