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

import Control.Monad
import Data.Function
import Data.Functor
import Foreign.C.Types

import Graphics.GL.Core40
import SDL

import Misc.AABB
import Graphics.GL.Primitives2D

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
  app glContext window
  putStrLn "Shutting down!"
  -- destroyRenderer r
  destroyWindow window
  quit


data Colors = Red | Green | Blue
  deriving (Eq, Ord, Show)

nextColor :: Colors -> Colors
nextColor Red = Green
nextColor Green = Blue
nextColor Blue = Red


app :: GLContext -> Window -> IO ()
app gl win = do
  rect <- newRectangle

  let red = V4 255 0 0 255
  rectColors rect (V4 red red red red)

  let corners = V4
        (V2 280 220)
        (V2 360 220)
        (V2 360 260)
        (V2 280 260)
  rectCoords rect corners

  fix \loop -> do
    evts <- pollEvents
    let shouldQuit = any isQuitEvent evts
    glMakeCurrent win gl
    glClearColor 0.0 0.0 0.0 1.0
    glClear GL_COLOR_BUFFER_BIT
    rectDraw rect
    glSwapWindow win
    unless shouldQuit loop

isQuitEvent :: Event -> Bool
isQuitEvent e = case eventPayload e of
  KeyboardEvent ke
    -> keyboardEventKeyMotion ke == Pressed
    && keysymKeycode (keyboardEventKeysym ke) == KeycodeQ
  QuitEvent -> True
  _ -> False