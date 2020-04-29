{-
    Copyright: 2020 Nicolas Stamm

    This file is part of Perfect Flow.

    Perfect Flow is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Perfect Flow is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Perfect Flow.  If not, see <https://www.gnu.org/licenses/>.
-}
{-|
Module      :   PF.Main
Description :   Main executable of the game.
Copyright   :   2020 Nicolas Stamm
License     :   GPL-3.0-or-later
-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module PF.Main where

import Control.Monad.Fix
import Data.Functor
import Foreign.C.Types

import Reflex
import Reflex.SDL2 as RS
import SDL

import Misc.AABB
import Reflex.SDL2.UI.Button
import Reflex.SDL2.UI.Layer

pfMain :: IO ()
pfMain = do
  putStrLn "Initializing SDL2..."
  initializeAll
  let ogl = defaultOpenGL { glProfile = Core Normal 3 3 }
  let wincfg = defaultWindow
        { windowOpenGL = Just ogl
        , windowResizable = True
        , windowHighDPI = False
        , windowInitialSize = V2 640 480
        }
  window <- createWindow "Perfect Flow" wincfg
  glContext <- SDL.glCreateContext window
  RS.host $ runLayers glContext window pfApp
  putStrLn "Shutting down!"
  destroyRenderer r
  destroyWindow window
  quit

pfApp :: (PerformEvent t m, MonadIO (Performable m), HasSDL2Events t m, MonadHold t m, MonadFix m, DynamicWriter t (DrawLayer m) m) => m ()
pfApp = mdo
  colorCyclingButton
  click <- getMouseButtonEvent
  -- performEvent_ $ click <&> liftIO . print
  shutdownOn =<< getQuitEvent

data Colors = Red | Green | Blue
  deriving (Eq, Ord, Show)

nextColor :: Colors -> Colors
nextColor Red = Green
nextColor Green = Blue
nextColor Blue = Red

colorCyclingButton ::
  ( PerformEvent t m
  , MonadIO (Performable m)
  , HasSDL2Events t m
  , MonadHold t m
  , MonadFix m
  , DynamicWriter t (DrawLayer m) m
  ) => m (Button t)
colorCyclingButton = mdo
  let aabb = AABB (P (V2 100 100)) (P (V2 300 200))
      aabbVertices = let
        [tl, tr, br, bl] = aabbCorners aabb
        in concat
          [ [tl, tr, bl]
          , [tr, br, bl]
          ]

  -- Allocate GL objects
  vao <- alloca $ glGenVertexArrays 1
  [vboPositions, vboColors] <- allocaArray 2 $ glGenBuffers 2

  -- Write vertex positions, since they do not change
  glBindVertexArray vao
  glBindBuffer GL_ARRAY_BUFFER vboPositions
  withArrayLen @Float (realToFrac <$> concatMap toList aabbVertices) $ \len ptr ->
    glBufferData GL_ARRAY_BUFFER (fromIntegral len) ptr GL_STATIC_DRAW
  glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE 0 0
  glEnableVertexAtrribArray 0
  

  currentColor <- foldDyn (const nextColor) Red clickEvt
  let 
    cfg = ButtonCfg
      { buttonCfgDraw = drawButton vao vbo aabb <$> currentColor
      , buttonCfgHitAABB = pure (fromIntegral <$> aabb)
      }
  btn@Button{ buttonClick = clickEvt } <- buildButton cfg
  pure btn

drawButton :: MonadIO m => AABB V2 CInt -> Colors -> ButtonStatus -> Draw m ()
drawButton vao vbo aabb color status = do
  [ Draw \r -> do
      rendererDrawColor r $= case color of
        Red -> V4 255 0 0 255
        Green -> V4 0 255 0 255
        Blue -> V4 0 0 255 255
      fillRect r (Just $ aabbToRect aabb)
  , Draw \r -> do
      rendererDrawColor r $= case status of
        ButtonUp -> V4 128 128 128 255
        ButtonHover -> V4 255 255 255 255
        ButtonDown -> V4 0 0 0 255
      drawRect r (Just $ aabbToRect aabb)
  ]
