{-# language OverloadedStrings #-}
module Game.Flow.Resources where

import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Traversable

import qualified Data.Text.IO as Text

import Apecs

import Apecs.Util
import Apecs.EntityIndex
import Data.Monoid.Extra
import Game.Flow.Components

{- | NOTE: getUnsafe is used only on entities that are known to have the requested components as noted in their type. The one use of @getUnsafe . cast@ is safe because the @cast@ is a down-cast. -}
tryStartCasting
  :: HasAll w '[Name, Castable, Casting, ResAmount, ResBounds]
  => Name
  -> System w ()
tryStartCasting spellName = do
  spellsFound <- lookupEntity spellName
  for_ spellsFound $ \spell -> do
    alreadyCasting <- exists @_ @Casting (cast spell)
    unless alreadyCasting $ do
      (Castable _time cost _dir) <- getUnsafe (cast spell @Castable) {- safe, see NOTE -}
      (LiftMonoid spendCosts, All canSpend) <- fmap fold . for cost $ \case
        (Self     , amount) -> resolveCanSpend (cast spell) amount
        (Other ref, amount) -> lookupEntity (Name ref) >>= \case
          Nothing  -> pure mempty
          Just res -> resolveCanSpend (cast res) amount
      when canSpend $ do
        spendCosts
        set spell (Casting 0)
 where
  resolveResourceCost :: HasAll w '[ResBounds, ResAmount] => Entity (ResBounds, ResAmount) -> AmountSpec -> System w Double
  resolveResourceCost ety = \case
    Max     -> maybe 0 resBoundsMax . fst . getSafe <$> get ety
    Min     -> maybe 0 resBoundsMin . fst . getSafe <$> get ety
    Current -> maybe 0 getResAmount . snd . getSafe <$> get ety
    Fixed x -> pure x
  resolveCanSpend :: HasAll w '[ResBounds, ResAmount] => Entity (ResBounds, ResAmount)-> AmountSpec -> System w (LiftMonoid (System w) (), All)
  resolveCanSpend resource cost = do
    toSpend <- resolveResourceCost resource cost
    current <- maybe 0 getResAmount . snd . getSafe <$> get resource
    pure
      ( lift $ modify (cast resource) (ResAmount . subtract toSpend . getResAmount)
      , All $ toSpend <= current
      )

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