-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GamePlay {player = p}) = 
  pictures [speler p]
  where
    speler (Player _ _ (x,y)) = color green (translate x y (circle 10))