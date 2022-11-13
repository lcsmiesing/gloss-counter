{-# language NamedFieldPuns #-}

-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Graphics.Gloss.Data.Vector
import Control.Monad (when)

runGame gstate@(GamePlay {player = p@(Player _ _ ppos lives),
                          bullets = b,
                          asteroids = a, 
                          obstacles = ob,
                          enemies = em,
                          animations = anims,
                          points = points}) 
  | lives /= 0 = gstate {player = updatePlayer p, bullets = concatMap updateBull b, asteroids = newas, enemies = map updateEnemies em, animations = newAnimations, points = updatePoints}
  | otherwise  = (GameOver {points = points})
  where
    updateBull bullet@(Bullet vel pos f) | bulletHit bullet a = [] --if bullet hits asteroid it is deleted from the list of bullets
                                         | otherwise   = [Bullet newvel (newvel .+ pos) f]
                                         where
                                          newvel = vel *.* foldr ((*.*) . collision pos 2) (1, 1) ob --new trajectory is set in motion if the bullet hits an obstacle
    updateEnemies enemy@(Enemy vel pos) = Enemy newVelocity (edgeDetection gstate(newVelocity .+ pos))
                                        where
                                          collenem = foldr ((*.*) . collision pos 10) (1, 1) ob--same here
                                          newVelocity | collenem /= (1,1) = vel *.* collenem
                                                      | otherwise = rot ppos pos vel --new velocity is calculated from player position
    updateAsteroid asteroid@(Asteroid vel pos s) | asteroidHit asteroid b && s > 5 = splitAsteroid asteroid --Asteroid is split if it is hit and still big enougn
                                                 | asteroidHit asteroid b          = [] --if not big enough, deleted form the list
                                                 | otherwise                       = [Asteroid newVelocity (edgeDetection gstate (newVelocity .+ pos)) s]--else the velocity is calculated the same as above
                                                where
                                                  newVelocity = vel *.* foldr ((*.*) . collision pos s) (1, 1) ob
    newas = concatMap updateAsteroid a
    zippie = zip newas a
    changes = [pos | (Asteroid vel1 pos _ ,Asteroid vel2 _ _ ) <- zippie, vel1 /= vel2] --for every asteroid, gets their (potentially updated) direction vector, their lastbounce, their position and lastly a value that tells us if the asteroid hit an obstacle
    newAnimations = map (\(Animation x y z) -> Animation x (y-1) z) (anims ++ map (\x -> Animation x 360 "") changes)
    playerDisFromEnemies = map (dis ppos . pose) em --Distance from enemies
    updatePoints | any (<2) playerDisFromEnemies = points - 0.5 --If a player is near the purple enemies, the score decreases
                 | otherwise = points + 0.005 --If not, the score increases with time
    updatePlayer speler@(Player vel ang pos liv) = Player (newVel (collplay pos *.* vel)) ang (edgeDetection gstate ((collplay pos *.* vel) .+ pos)) (setLives liv)
    setLives lives | playerHit p a = lives - 1 --If the player is hit by an asteroid its lives decreases
                   | otherwise     = lives
    collplay p = foldr ((*.*) . collision p 10) (1, 1) ob --detect if a player has a collision with an
    newVel v | magV v > 10 = 10 .* norm v --Player velocity is limited to a certain degree
             | otherwise   = v
-- | Handle one iteration of the game, if paused nothing happens except for angle movement of the player
step :: Float -> GameState -> IO GameState
step secs gstate@(GamePlay {isPaused = p})
  | not p
  = -- We show a new random number
    do return $ runGame gstate {elapsedTime = elapsedTime gstate + secs}
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }
step secs gstate@(GameOver {points = points}) = return gstate {points = points}

--If something reaches the border of the screen the coordinates are flipped in a way the point is on the other side of the screen
edgeDetection :: GameState -> Point -> Point
edgeDetection GamePlay {gameBorders = ((a,b),(c,d)), obstacles = ob} (x,y) 
                              | x <= a-20 = (abs x, y)
                              | x >= b+20 = (-x, y)
                              | y <= c-20 = (x, abs y)
                              | y >= d+20 = (x, -y)
                              | otherwise = (x,y)
edgeDetection GameOver {points} p = p


--This function takes a point size and certain obstacle to determine the vector a certain "object" needs to change direction to
collision :: Point -> Float -> Obstacle -> Vector
collision p s (Obstacle p' w h ) | xdist > w/2 + s  = (1,1) --if the point is on one of either left or right side of the obstacle but not colliding
                                 | ydist > h/2 + s  = (1,1) --if the point is on one of either down or up side of the obstacle but not colliding
                                 | xdist <= w/2   = (1,-1) --Does collide
                                 | ydist <= h/2     = (-1,1) --vice versa
                                 | corner <= s**2 = (-1,-1)
                                 | otherwise      = (1,1)
                                           where
                                             xdist = abs $ x - a --distance between x-axis centres
                                             ydist = abs $ y - b --distance between y-axis centres
                                             corner = (xdist - w/2)**2 + (ydist - h/2)**2 --calculate the distance from centre to corner
                                             (x,y) = p
                                             (a,b) = p' 

--Determine if a bullet has a collision with any of the asteroids
bulletHit :: Bullet -> [Asteroid] -> Bool
bulletHit (Bullet _ pos _) = any ((True ==) . checkAsteroids)--Check if a bullet collides with any of the asteroids
                                        where
                                          checkAsteroids (Asteroid _ position size) | dis pos position < size = True --If position is within range "size"
                                                                                    | otherwise               = False

--Determine if an asteroid has a collision with any of the bullets
asteroidHit :: Asteroid -> [Bullet] -> Bool
asteroidHit (Asteroid _ pos size) = any ((True ==) . checkBullets) --logic is similar as the function above
                                        where
                                          checkBullets (Bullet _ p _) | dis pos p < size = True
                                                                      | otherwise        = False

--Determine if the player has a collision with any of the asteroids
playerHit :: Player -> [Asteroid] -> Bool
playerHit (Player _ _ pos _) = any ((True ==) . checkPlayer)
                                        where
                                          checkPlayer (Asteroid _ position size) | dis pos position < size + 5 = True
                                                                                 | otherwise                   = False

--Depending on the asteroid, create 3 smaller faster asteroids that go in different directions
splitAsteroid :: Asteroid -> [Asteroid]                                                                          
splitAsteroid (Asteroid (x,y) pos size) = [Asteroid (x*1.5,y*1.5) pos (size/3),Asteroid (x*(-1.5),y*1.5) pos (size/3),Asteroid (x*(-1.5),y*(-1.5)) pos (size/3)]

--Return input IO GameState                        
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

--Press p to change pause flag
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'p') Down _ _) gstate@(GamePlay {isPaused})
    | isPaused  = gstate {isPaused = False}
    | otherwise = gstate {isPaused = True}

--When the "wasd" keys are pressed change the velocity of the player depending on the key
inputKey (EventKey (Char c) Down _ _) gstate@(GamePlay {player = Player vel ang pos liv})
  = let direction = case c of 
                      'w' -> (0, 1)
                      'a' -> (-1, 0)
                      's' -> (0, -1)
                      'd' -> (1, 0)
                      _   -> (0, 0)
    in
      gstate {player = Player (vel .+ direction) ang pos liv}
        
--When the game is not paused and the player clicks the mouse a bullet is shot in the direction of the mouse
inputKey (EventKey (MouseButton LeftButton) Down _ clickPos)
  gstate@(GamePlay {player = p@(Player _ _ posp _), bullets, isPaused})
    | isPaused  = gstate 
    | otherwise = gstate {bullets = bullets ++ [newBullet]}
    where
      newBullet = Bullet bulletVel posp False
      bulletVel =  8 .* norm (clickPos .- posp)

--When the mouse moves, point the player in the direction of the mouse by changing the angle
inputKey (EventMotion (x,y)) gstate@(GamePlay {player = (Player velp angle posp@(a,b) liv)}) 
  = gstate {player = Player velp (argV (x-a,y-b)) posp liv}

--If no key is pressed keep the same
inputKey _ gstate = gstate

---Add points and vectors
(.+) :: Point -> Point -> Point
(x,y) .+ (a,b) = (x+a, y+b)

--Substract points and vectors
(.-) :: Point -> Point -> Point
(x,y) .- (a,b) = (x-a, y-b)

--Multiply float by a vector
(.*) :: Float -> Vector -> Vector
s .* (x,y) = (s*x,s*y)

--Dotproduct of 2 vectors
(*.*) :: Vector -> Vector -> Vector
(a,b) *.* (x,y) = (a*x,b*y)

--Normalize a vector
norm :: Vector -> Vector
norm (x,y) = let m = magn (x,y) in (x/m,y/m)

--Magnitude of a vector
magn :: Vector -> Float
magn (x,y) = sqrt(x**2 + y**2)

--rotates a vector incrementially directed at a certain point
rot :: Point -> Point -> Vector -> Vector
rot p1 p2 vec = res
    where
      dirv = norm (p1 .- p2)
      res = norm (vec .+ (0.05 .* dirv))

--Calculates distance between two points
dis :: Point -> Point -> Float
dis (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2
