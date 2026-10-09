{-
    Copyright: 2026 Nimue Eris Wesemael

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
Copyright   :   2026 Nimue Eris Wesemael
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
import PF.AppM
import Control.Monad.Reader
import Data.Word

pfMain :: IO ()
pfMain = do
  putStrLn "Initializing SDL2..."
  initializeAll
  let ogl = defaultOpenGL { glProfile = Core Normal 3 3 }
  let wincfg = defaultWindow
        { windowGraphicsContext = OpenGLContext ogl
        , windowResizable = True
        , windowHighDPI = False
        , windowInitialSize = V2 640 480
        }
  window <- createWindow "Perfect Flow" wincfg
  renderer <- createRenderer window 0 defaultRenderer
  runAppWith window renderer app
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


app :: AppM ()
app = do
  rdr <- asks (.renderer)

  let red = V4 255 0 0 255
  let black = V4 0 0 0 255

  let rect = aabb2rect $ mkAABB
        (P $ V2 280 220)
        (P $ V2 360 260)

  fix \loop -> do
    evts <- pollEvents
    let shouldQuit = any isQuitEvent evts

    -- draw some stuff
    rendererDrawColor rdr $= black
    clear rdr
    rendererDrawColor rdr $= red
    fillRect rdr (Just rect)

    present rdr
    -- loop
    unless shouldQuit loop

isQuitEvent :: Event -> Bool
isQuitEvent e = case eventPayload e of
  KeyboardEvent ke
    -> keyboardEventKeyMotion ke == Pressed
    && keysymKeycode (keyboardEventKeysym ke) == KeycodeQ
  QuitEvent -> True
  _ -> False

aabb2rect :: AABB V2 CInt -> Rectangle CInt
aabb2rect (AABB lo hi) = Rectangle lo (hi .-. lo)