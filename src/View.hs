-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GamePlay {player = p, bullets = b, obstacles = ob}) = 
  pictures [speler p, kogel b, obs ob]
  where
    --speler (Player _ _ (x,y)) = color green (translate x y (polygon [(0,50), (-25,0), (25,0)]))
    speler (Player _ _ (x,y)) = color green (translate x y (circle 10))
    kogel b = pictures [translate x y (color white (circle 2))
                         | Bullet _ (x,y) _ <- b]
    obs ob = pictures [color orange (translate x y (rectangleSolid w h)) 
                         | Obstacle (x,y) w h <- ob]