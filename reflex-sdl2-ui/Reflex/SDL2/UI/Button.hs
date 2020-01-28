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
Module      :   Reflex.SDL2.UI.Button
Description :   A simple clickable "button" widget.
Copyright   :   2020 Nicolas Stamm
License     :   GPL-3.0-or-later
-}
module Reflex.SDL2.UI.Button where

import Reflex
import Reflex.SDL2

import Misc.AABB
import Reflex.SDL2.UI.Layer

data ButtonStatus = ButtonDown | ButtonHover | ButtonUp
  deriving (Eq, Ord, Show)

data Button t = Button
  { buttonStatus :: Dynamic t ButtonStatus
  , buttonClick :: Event t (V2 Double)
  }

data ButtonCfg t m = ButtonCfg
  { buttonCfgDraw :: Dynamic t (ButtonStatus -> DrawLayer m)
  , buttonCfgHitAABB :: Dynamic t (AABB V2 Int)
  }

defaultButton :: Reflex t => ButtonCfg t m
defaultButton = ButtonCfg
  { buttonCfgDraw = constDyn mempty
  , buttonCfgHitAABB = zeroAABB
  }

buildButton :: ButtonCfg t m -> m (Button t)
buildButton ButtonCfg{buttonCfgDraw, buttonCfgHitAABB} = do

  emouseMove <- getMouseMotionEvent
  emouseClick <- getMouseButtonEvent

  let processClick evt prev = do
        aabb <- sample buttonCfgHitAABB
        case mouseButtonEventMouseButton evt of
          ButtonLeft -> case mouseButtonEventInputMotion evt of
            Pressed -> case fmap fromIntegral (mouseButtonEventPos evt) `posWithin` fmap fromIntegral aabb of
              Just relpos -> pure (prev <|> Just relpos)
              Nothing -> pure Nothing
            Released -> pure Nothing
          _ -> pure prev
  disClicked <- holdUniqDyn =<< foldDynM processClick Nothing emouseClick

  disInside <- holdUniqDyn =<< holdDyn =<< attachWith (flip inAABB) (current buttonCfgHitAABB) emouseMove

  let dStatus = ffor2 disClicked disInside \clicked inside -> if inside
        then case clicked of
          Just _ -> ButtonDown
          Nothing -> ButtonHover
        else ButtonDown
  
  let eClick = fmapMaybe id (updated disClicked)
  pure Button
    { buttonStatus = dStatus
    , buttonClick = eClick
    }