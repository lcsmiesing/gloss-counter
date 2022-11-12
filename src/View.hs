-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Controller
import Graphics.Gloss.Data.Vector
import Data.Fixed

view :: GameState  -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GamePlay {player = p, bullets = b, obstacles = ob, asteroids = as, enemies = em, animations = am, elapsedTime = e}) = 
  pictures [speler p, kogel b, obs ob, ast as, ene em, ani am, elap e]
  where
    --speler (Player _ _ (x,y)) = color green (translate x y (polygon [(0,50), (-25,0), (25,0)]))
    speler (Player _ angle (x,y)) = color green (translate x y (polygon (map (rotateV angle) [(-3,-5), (-5,0), (-3,5), (10,0)])))
    kogel b = pictures [translate x y (color white (circle 2))
                         | Bullet _ (x,y) _ _ <- b]
    obs ob = pictures [color orange (translate x y (rectangleSolid w h)) 
                         | Obstacle (x,y) w h d <- ob]
    ast as = pictures ([color white (translate v w (circleSolid s))
                         | Asteroid (x,y) (v,w) s f <- as] ++ [color white (line [posa,posa.+(50.*vela)])
                         | Asteroid vela posa s f <- as])
    ene em = pictures [color red (arrow [posa,posa.+(15.*norm vela)]) | Enemy vela posa s <- em]
    ani am = pictures [color blue (translate x y(circle (37.5 - (d/10))))| Animation (x,y) d s <- am, d > 0]
    elap e = pictures [translate (-275) 275 (scale 0.1 0.1(color green (text ("score: " ++(show (round e))))))]
-- two functions below are taken from:
-- https://github.com/ivanperez-keera/SoOSiM-ui/blob/master/src/Graphics/Gloss/AdvancedShapes/Arrows.hs
arrow :: Path -> Picture
arrow p 
  | [p1, p2] <- p, p1 /= p2
  , (v1, v2) <- calcArrowHeadVertex (p1, p2)
  = color red $ Pictures [ line [p1, p2]
                                , polygon [p2, v1, v2]
                                ]
arrow _ = blank

calcArrowHeadVertex :: (Point, Point) -> (Point, Point)
calcArrowHeadVertex ((start_x, start_y), (end_x, end_y)) = ((x1, y1), (x2, y2))
  where angle = atan2 (end_y - start_y) (end_x - start_x)
        x1 = end_x - 10 * cos(angle - 50)
        y1 = end_y - 10 * sin(angle - 50)
        x2 = end_x - 10 * cos(angle + 50)
        y2 = end_y - 10 * sin(angle + 50)
