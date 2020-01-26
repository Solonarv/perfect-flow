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

{-# LANGUAGE OverloadedStrings #-}
module PF.Main where

import Reflex.SDL2 as RS

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
  RS.host pfApp
  putStrLn "Shutting down!"
  destroyWindow window
  quit

pfApp :: ConcreteReflexSDL2 ()
pfApp = do
  shutdownOn =<< getQuitEvent