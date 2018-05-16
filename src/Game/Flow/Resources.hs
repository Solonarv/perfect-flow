module Game.Flow.Resources where

import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Traversable

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
