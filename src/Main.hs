{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language GeneralizedNewtypeDeriving #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Traversable

import Apecs
import qualified Apecs.Slice as Slice
import SDL hiding (get)
import qualified SDL

import Data.Text (Text)

newtype ResAmount = ResAmount { getResAmount :: Double } deriving Show
instance Component ResAmount where type Storage ResAmount = Map ResAmount

data ResBounds = ResBounds { resBoundsMin :: Double, resBoundsMax :: Double } deriving Show
instance Component ResBounds where type Storage ResBounds = Map ResBounds

newtype ResRegen = ResRegen { getResRegen :: Double } deriving Show
instance Component ResRegen where type Storage ResRegen = Map ResRegen

data Castable = Castable { castCost :: Double, castTime :: Double } deriving Show
instance Component Castable where type Storage Castable = Map Castable

newtype Casting = Casting { castingProgress :: Double } deriving Show
instance Component Casting where type Storage Casting = Unique Casting

data Damage = Damage { getDamage :: Double } deriving Show
instance Component Damage where type Storage Damage = Map Damage

newtype Name = Name { getName :: Text } deriving (Show, Eq)
instance Component Name where type Storage Name = Map Name

newtype DamageDealt = DamageDealt { getDamageDealt :: Sum Double } deriving (Show, Monoid)
instance Component DamageDealt where type Storage DamageDealt = Global DamageDealt

newtype Time = Time { getTime :: Sum Double } deriving (Show, Monoid)
instance Component Time where type Storage Time = Global Time

makeWorld "World" [ ''ResAmount
                  , ''ResBounds
                  , ''ResRegen
                  , ''Castable
                  , ''Casting
                  , ''Name
                  , ''Damage
                  , ''DamageDealt
                  , ''Time
                  ]
type System' = System World

main :: IO ()
main = do
  initializeAll
  world <- initWorld
  window <- createWindow "Perfect Flow" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  runSystem initializeEntities world
  runWith world $ mainLoop renderer
  runWith world $ getGlobal >>= liftIO . putStrLn . ("Damage dealt: "++) . show . getSum . getDamageDealt

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
  render renderer
  tick 1
  when (not $ or shouldExit) $ do
    fixFrameTime (1/30)
    mainLoop renderer

fixFrameTime :: Double -> System' ()
fixFrameTime desiredFrameTime = do
  lastFrame <- getSum . getTime <$> getGlobal
  now <- time
  let elapsed = now - lastFrame
      remaining = desiredFrameTime - elapsed
  liftIO $ when (remaining > 0) $ threadDelay (round $ remaining * 1e6)

-- NOTE: getUnsafe is used only on entities that are known to have the requested components
-- as noted in their type. The one use of @getUnsafe . cast@ is safe because the @cast@ is a down-cast.
tryStartCasting :: Name -> System' ()
tryStartCasting spellName = do
  named <- owners @(Name, Castable, ResAmount) >>= Slice.filterM (fmap (==spellName) . getUnsafe . cast)
  for_ (Slice.toList named) $ \toCast -> do
    alreadyCasting <- exists @_ @Casting (cast toCast)
    when (not alreadyCasting) $ do
      (_, Castable cost time, ResAmount energy) <- getUnsafe toCast -- safe, see NOTE
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
        -- Name nm <- fromMaybe (Name "") . getSafe <$> get (cast res @Name)
        liftIO $ renderBar (Rectangle topleft (V2 300 50)) $ (amt - lo) / (hi - lo)
    renderCasting = do
      cmapM_ $ \(Castable _ time, Casting progress) -> do
        liftIO $ renderBar (Rectangle (P (V2 300 400)) (V2 300 50)) (progress / time)
    renderBar bbox@(Rectangle pt bounds@(V2 w h)) progress = do
      rendererDrawColor r $= V4 0 0 0 0
      drawRect r (Just bbox)
      rendererDrawColor r $= V4 130 0 0 20
      fillRect r . Just $ Rectangle (pt + pure 1) (V2 (round $ (fromIntegral $ w-2) * progress) (h-2))
        


initializeEntities :: System' ()
initializeEntities = do
  setGlobal (DamageDealt 0)
  newEntity (Name "Strike", ResAmount 100, ResBounds 0 100, ResRegen 0, Castable 100 30 )
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
    resolveCasting = cimapM_ $ \(e, (Casting progress, Castable cost casttime)) ->
      when (progress >= casttime) $ do
        destroy $ cast e @Casting
        get (cast e @Damage) >>= \case
          Safe Nothing -> pure ()
          Safe (Just (Damage dmg)) -> modifyGlobal $ mappend $ DamageDealt (Sum dmg)


clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = min hi (max x lo)