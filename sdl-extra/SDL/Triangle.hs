module SDL.Triangle (Triangle(..), triangle, triangleFan) where

import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Functor
import           Data.List              (sortOn)
import           Foreign.C.Types        (CInt)

import           Control.Lens
import           Data.Vector            (Vector)
import qualified Data.Vector            as Vector
import           Linear
import           SDL.Vect               hiding (Vector)
import           SDL.Video.Renderer

import           SDL.Monad.Renderer

data Triangle = Tri !(Point V2 CInt) !(Point V2 CInt) !(Point V2 CInt)

triangle :: MonadRenderer m => Triangle -> m ()
triangle (Tri p0 p1 p2) = do
  rdr <- getRenderer
  liftIO $ triangle_ rdr (fromIntegral <$> p0) (fromIntegral <$> p1) (fromIntegral <$> p2)
{-# inline triangle #-}

triangle_ :: Renderer -> Point V2 Double -> Point V2 Double -> Point V2 Double -> IO ()
triangle_ rdr (P v0) (P v1) (P v2)
  | v0^._y == v1^._y
    = flatTriangle rdr v2 v0 v1
  | v0^._y == v2^._y
    = flatTriangle rdr v1 v0 v2
  | v1^._y == v2^._y
    = flatTriangle rdr v0 v1 v2
  | (lo, md, hi) <- sortTriByY v0 v1 v2
  , dx           <- hi^._x - lo^._x
  , splity       <- md^._y
  , k            <- (splity - lo^._y) / (hi^._y - lo^._y)
  , splitx       <- lo^._x + (hi^._x - lo^._x) * k
  , split        <- V2 splitx splity
    = flatTriangle rdr hi md split >> flatTriangle rdr lo md split

flatTriangle :: Renderer -> V2 Double -> V2 Double -> V2 Double -> IO ()
flatTriangle rdr apex v1 v2 = for_ lines $ \(x1, x2, y) -> drawLine rdr (round <$> P (V2 x1 y)) (round <$> P (V2 x2 y))
  where
    flaty = v1^._y
    apexX = apex^._x
    apexY = apex^._y
    height = flaty - apexY
    islope1 = (v1^._x - apexX) / height
    islope2 = (v2^._x - apexX) / height
    lines =
      [ (y * islope1 + apexX, y * islope2 + apexX, apexY + y)
      | y <- (* signum height) <$> [0..(abs height)]
      ]
{-# inline flatTriangle #-}

sortTriByY :: Ord a => V2 a -> V2 a -> V2 a -> (V2 a, V2 a, V2 a)
sortTriByY v0 v1 v2 = let [l, m, h] = sortOn (view _y) [v0, v1, v2] in (l, m , h)
{-# inline sortTriByY #-}

triangleFan :: (Foldable f, MonadRenderer m) => Point V2 CInt -> f (Point V2 CInt) -> m ()
triangleFan apex = void . foldr
    (\point mprev -> mprev >>= \case
      Nothing -> pure (Just point)
      Just prev -> do
        triangle (Tri apex prev point)
        pure (Just point)
    )
    (pure Nothing)
