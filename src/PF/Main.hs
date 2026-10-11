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
import SDL.Font qualified as Font

import Misc.AABB
import PF.AppM
import Control.Monad.Reader
import Data.Word
import PF.Simulation

import PF.Levels.ArcaneWizard qualified
import PF.Render
import Data.IORef
import Data.Foldable
import Data.Maybe
import Control.Concurrent (threadDelay)

pfMain :: IO ()
pfMain = do
  putStrLn "Initializing SDL2..."
  initializeAll
  Font.initialize
  let ogl = defaultOpenGL { glProfile = Core SDL.Normal 3 3 }
  let wincfg = defaultWindow
        { windowGraphicsContext = OpenGLContext ogl
        , windowResizable = True
        , windowHighDPI = False
        , windowInitialSize = V2 640 480
        }
  window <- createWindow "Perfect Flow" wincfg
  renderer <- createRenderer window 0 defaultRenderer
  opensans12 <- Font.load "data/assets/fonts/OpenSans-Regular.ttf" 12
  runAppWith window renderer PF.Levels.ArcaneWizard.initWorld opensans12 app
  putStrLn "Shutting down!"
  destroyRenderer renderer
  destroyWindow window
  Font.free opensans12
  Font.quit
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
    start <- time
    evts <- pollEvents
    let shouldQuit = any isQuitEvent evts

    let instructions = mapMaybe parseEvent evts
    unless (null instructions) do
      liftIO $ print instructions
    doInstruction StepTime
    for_ instructions doInstruction

    -- draw some stuff
    render

    present rdr
    -- loop
    end <- time
    let elapsed = end - start
        remaining = 1e-3*(16+2/3) - elapsed
    liftIO . threadDelay $ floor $ remaining * 1e6
    unless shouldQuit loop

isQuitEvent :: Event -> Bool
isQuitEvent e = case eventPayload e of
  KeyboardEvent ke
    -> keyboardEventKeyMotion ke == Pressed
    && keysymKeycode (keyboardEventKeysym ke) == KeycodeQ
  QuitEvent -> True
  _ -> False

data Instruction = Cast Int Bool | StepTime
  deriving (Eq, Ord, Show)

parseEvent :: Event -> Maybe Instruction
parseEvent = \case
  Event{eventPayload = KeyboardEvent ke}
    | ke.keyboardEventKeyMotion == Pressed
    , let keycode = ke.keyboardEventKeysym.keysymKeycode
    -> if
        -- | keycode == KeycodeSpace -> Just StepTime
        | keycode >= Keycode0 && keycode <= Keycode9
        , i <- if keycode == Keycode0
            then 10
            else fromIntegral $ keycode.unwrapKeycode - (Keycode0).unwrapKeycode - 1
        , mod <- ke.keyboardEventKeysym.keysymModifier
        , let force = mod.keyModifierLeftShift || mod.keyModifierRightShift
        -> Just $ Cast i force
        | otherwise -> Nothing
  _ -> Nothing

doInstruction :: Instruction -> AppM ()
doInstruction = \case
  StepTime -> do
    world <- asks (.world)
    liftIO $ modifyIORef world step
  Cast i force -> do
    worldRef <- asks (.world)
    world <- liftIO $ readIORef worldRef
    let ability = case drop i (toList world.abilities) of
          (x:_) -> Just x
          _ -> Nothing
        int = if force then Interrupt_Do else Interrupt_Don't
    for_ ability \ability ->
      liftIO $ modifyIORef worldRef (cast ability.name int)


aabb2rect :: AABB V2 CInt -> Rectangle CInt
aabb2rect (AABB lo hi) = Rectangle lo (hi .-. lo)