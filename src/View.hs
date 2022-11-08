-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GamePlay {player = p, bullets = b}) = 
  pictures [speler p, kogel b]
  where
    --speler (Player _ _ (x,y)) = color green (translate x y (polygon [(20,0),(0,20),(10,20)]))
    speler (Player _ _ (x,y)) = color green (translate x y (circle 10))
    kogel b = pictures [translate x y (color white (circle 2))
                         | Bullet _ (x,y) _ <- b]