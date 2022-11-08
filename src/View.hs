-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GamePlay {player = p, bullets = b, obstacles = ob}) = 
  pictures [speler p, kogel b, obs ob]
  where
    speler (Player _ angle (x,y)) = color green (translate x y (polygon (map (rotateV angle) [(-3,-5), (-5,0), (-3,5), (10,0)])))
    kogel b = pictures [translate x y (color white (circle 2))
                         | Bullet _ (x,y) _ <- b]
    obs ob = pictures [color orange (translate x y (rectangleSolid w h)) 
                         | Obstacle (x,y) w h <- ob]