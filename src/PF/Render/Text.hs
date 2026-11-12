module PF.Render.Text where

import SDL qualified
import SDL.Font qualified as SDL
import Data.HashMap (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import PF.Data.Symbol as Sym
import Data.StateVar
import Control.Monad.State.Strict

data GlyphAtlas = GlyphAtlas
  { mappings :: MVar (HashMap Glyph GlyphLoc)
  , pages :: IORef (Vector SDL.Texture)
  , font :: SDL.Font
  }

data Glyph = Glyph
  { glyph :: !Text
  , color :: !Font.Color
  }

data GlyphLoc = GlyphLoc
  { ix :: !Int
  , rect :: !(SDL.Rectangle CInt)
  }

prepareGlyphs :: GlyphAtlas -> HashSet Glyph -> Renderer -> IO ()
prepareGlyphs atlas glyphs' rdr = modifyMVar_ atlas.mappings \mappings -> do
  let glyphs = Vector.fromList $ HashSet.toList $ HashSet.filter (`HashMap.member` mappings) glyphs'
      chunks = List.unfoldr glyphs \v ->
        if Vector.null v
        then Nothing
        else Just $ Vector.splitAt 64 v
  nextIx <- liftIO $ Vector.size <$> readIORef atlas.pages
  (newPages, mappings') <- flip runStateT mappings
   $ for (zip [nextIx ..] chunks) \(ix, chunk) -> do
    (gdims, (w, h)) <- dimensions chunk
    surf <- SDL.createRGBSurface rdr SDL.RGBA8888 (V2 w h)
    SDL.textureBlendMode tex $= BlendAdditive
    let go x i = do
          let g = glyphs Vector.! i
              (w,h) = gdims Vector.! i
              rect = SDL.Rectangle (P (V2 x 0)) (V2 w h)
          gSurf <- Font.blended atlas.font g.color g.glyph
          SDL.surfaceBlit gSurf Nothing surf rect
          modify $ HashMap.insert (g, GlyphLoc ix rect)
          when (i < Vector.size glyphs-1)
            $ go (x+w) (i+1)
    go 0 0
    SDL.createTextureFromSurface rdr surf
  liftIO $ modifyIORef' atlas.pages (<> Vector.fromList newPages) 
  pure mappings'

dimensions :: SDL.Font -> Vector Glyph -> IO (Vector (CInt, CInt), (CInt, CInt))
dimensions font gs = flip runStateT (0,0) $ Vector.mapM go gs where
  go g = do
    (w, h) <- Font.size font g.glyph
    modify \(tw, mh) -> (w+tw, max h mh)
    pure (w, h)

blitText :: MonadIO m => GlyphAtlas -> SDL.Renderer -> Text -> m ()
blitText atlas rdr txt = error "AAAA how 2 split texts??"
