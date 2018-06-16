{-# LANGUAGE OverloadedStrings #-}
module Game.Flow.Resources where

import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable
import           Data.Monoid
import           Data.Traversable

import qualified Data.Text.IO         as Text

import           Apecs
import qualified Apecs.Slice          as Slice

import           Apecs.EntityIndex
import           Apecs.Extra
import           Data.Monoid.Extra
import           Game.Engine.Settings
import           Game.Flow.Components

-- | NOTE: getUnsafe is used only on entities that are known to have the requested components
-- as noted in their type. The one use of @getUnsafe . cast@ is safe because the @cast@ is a down-cast.
tryStartCasting
  :: HasAll w '[Name, Castable, Casting, ResAmount, ResBounds]
  => GameSettings
  -> Entity Castable
  -> System w ()
tryStartCasting settings spell = do
  castableInfo <- getSafe <$> get spell
  for_ castableInfo $ \(Castable _time cost _dir) -> do
    alreadyCasting <- exists @_ @Casting (cast spell)
    castingAnySpell <- not . Slice.null <$> owners @Casting
    unless (alreadyCasting || castingAnySpell && not (gameSettingsInterruptOnCast settings)) $ do
      (LiftMonoid spendCosts, All canSpend) <- fmap (foldMap $ bimap LiftMonoid All) . for cost $ \case
        (Self     , amount) -> resolveCanSpend (cast spell) amount
        (Other ref, amount) -> lookupEntity (Name ref) >>= \case
          Nothing  -> pure (pure (), amount /= Fixed 0)
          Just res -> resolveCanSpend (cast res) amount
      when canSpend $ do
        spendCosts
        set spell (Casting 0)
 where
  resolveCanSpend :: HasAll w '[ResBounds, ResAmount] => Entity (ResBounds, ResAmount)-> AmountSpec -> System w (System w (), Bool)
  resolveCanSpend resource cost = do
    toSpend <- resolveResourceCost resource cost
    current <- maybe 0 getResAmount . snd . getSafe <$> get resource
    pure
      ( modify (cast resource) (ResAmount . subtract toSpend . getResAmount)
      , toSpend <= current
      )

resolveResourceCost :: HasAll w '[ResBounds, ResAmount] => Entity (ResBounds, ResAmount) -> AmountSpec -> System w Double
resolveResourceCost ety = \case
  Max     -> maybe 0 resBoundsMax . fst . getSafe <$> get ety
  Min     -> maybe 0 resBoundsMin . fst . getSafe <$> get ety
  Current -> maybe 0 getResAmount . snd . getSafe <$> get ety
  Fixed x -> pure x

resolveCasting :: HasAll w '[Casting, Castable, Name, OnCastCompleted, DamageDealt] => System w ()
resolveCasting =
  cimapM_ $ \(e, (Casting progress, Castable casttime _cost _direction)) ->
    when (progress >= casttime) $ do
      destroy $ cast e @Casting
      getUnsafe (cast e @Name) >>= liftIO . \(Name n) -> Text.putStrLn $ "Finished casting " <> n
      get (cast e @OnCastCompleted) >>= traverse_ (\case
        OnCastCompleted acts -> for_ acts $ \case
          Damage dmg -> do
            liftIO . putStrLn $ "Dealt " ++ show dmg ++ " damage!"
            modifyGlobal $ mappend $ DamageDealt (Sum dmg)) . getSafe

advanceCasting :: HasAll w '[Casting] => Double -> System w ()
advanceCasting dT = cmap $ \(Casting progress) -> Casting (progress + dT)

regenResources :: HasAll w '[ResAmount, ResRegen] => Double -> System w ()
regenResources dT = rmap $ \(ResAmount amt, ResRegen reg) -> ResAmount (amt + reg * dT)

clampResources :: HasAll w '[ResAmount, ResBounds] => System w ()
clampResources = rmap $ \(ResAmount amt, ResBounds lo hi) -> ResAmount (clamp lo hi amt)

clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = min hi (max x lo)
