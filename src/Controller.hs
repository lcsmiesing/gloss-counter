{-# language NamedFieldPuns #-}

-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Graphics.Gloss.Data.Vector

runGame gstate@(GamePlay {player = p@(Player pvel pang ppos),
                          bullets = b,
                          asteroids = a, 
                          obstacles = ob,
                          enemies = em,
                          animations = anims,
                          points = points}) = gstate {player = updatePlayer p, bullets = map updateBull b, asteroids = newas, enemies = map updateEnemies em, animations = newAnimations, points = updatePoints}
  where
    updateBull (Bullet vel pos f s) = Bullet vel (vel .+ pos) f s
    gete em@(Enemy vel pos s) = diff (vel,s) (map (colle em) ob)
    updateEnemies em@(Enemy vel pos s) = Enemy (rot ppos pos (fst (gete em))) (edgeDetection gstate(vel .+ pos)) (snd (gete em))
    updateAsteroids as@(Asteroid vel pos s str) = Asteroid newVelocity (isHit (edgeDetection gstate (newVelocity .+ pos)) s b) s str
                                                where
                                                  newVelocity = asteroidHelper as
    asteroidHelper a@(Asteroid v _ _ _) | any (True==) $ map (collision a) ob = (-1) .* v
                                        | otherwise                       = v
    --updateAsteroid = map (\((x,y,z,zz),(Asteroid vel pos f s)) -> Asteroid x (isHit (edgeDetection gstate (vel .+ pos)) f b) f y) (zip updates a)
    newas = map updateAsteroids a
    zippie = zip newas a
    changes = [pos | ((Asteroid vel1 pos _ _),(Asteroid vel2 _ _ _)) <- zippie, vel1 /= vel2] --for every asteroid, gets their (potentially updated) direction vector, their lastbounce, their position and lastly a value that tells us if the asteroid hit an obstacle
    newAnimations = map (\(Animation x y z) -> (Animation x (y-1) z)) (anims ++ (map (\x -> Animation x 360 "") changes))
    playerDisFromEnemies = map (dis ppos . pose) em
    updatePoints | any (<1) playerDisFromEnemies = points - 0.5
                 | otherwise = points + 0.005
    updatePlayer (Player v a p) = Player (newVel v) a (newVel v .+ p)
    newVel v | magV v > 10 = 10 .* (norm v)
             | otherwise   = v

--(isHit (edgeDetection gstate (vel .+ pos)) f b)
-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GamePlay {isPaused = p})
  | not p
  = -- We show a new random number
    do return $ runGame gstate {elapsedTime = elapsedTime gstate + secs}
  | p
  =
    do return gstate
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }



edgeDetection :: GameState -> Point -> Point
edgeDetection GamePlay {gameBorders = ((a,b),(c,d)), obstacles = ob} (x,y) 
                              | x <= a-20 = (abs x, y)
                              | x >= b+20 = (-x, y)
                              | y <= c-20 = (x, abs y)
                              | y >= d+20 = (x, -y)
                              | otherwise = (x,y)
edgeDetection GameOver {points} p = p

--this function checks if an asteroid collides with a side of an obstacle
--if it does, it changes the asteroids direction vector to "bounce" off the side of the obstacle
--using a predefined reflection vector

collision :: Asteroid -> Obstacle -> Bool
collision (Asteroid _ (x,y) s _) (Obstacle (a,b) w h _) | xdist > w/2 + s  = False
                                                        | ydist > h/2 + s  = False
                                                        | xdist <= w/2   = True
                                                        | ydist <= h/2     = True
                                                        | corner <= s**2 = True
                                                        | otherwise      = False
                                                        where
                                                          xdist = abs $ x - a
                                                          ydist = abs $ y - b
                                                          corner = (xdist - w/2)**2 + (ydist - h/2)**2
--same as coll but for enemies  
colle :: Enemy -> Obstacle -> (Vector,String)
colle (Enemy v p f) (Obstacle _ _ _ (a,b,c,d)) | top && f /= "top" = (v .- ((2*dotproduct v (0,1)).*(0,1)),"top")
                                                       | left && f/= "left" = (v .- ((2*dotproduct v (-1,0)).*(-1,0)),"left")
                                                       | right && f/="right" = (v .- ((2*dotproduct v (1,0)).*(1,0)),"right")
                                                       | bottom && f/="bottom" = (v .- ((2*dotproduct v (0,-1)).*(0,-1)),"bottom")
                                                       | otherwise = (v,f)
  where
    top = distance a b p -- (0,1)
    left = distance a c p --(-1,0)
    right = distance b d p --(1,0)
    bottom = distance c d p --(0,-1)


diff :: (Vector,String) -> [(Vector,String)] -> (Vector,String)
diff v [] = v
diff v (x:xs) | v /= x = x
              | otherwise = diff v xs

isHit :: Point -> Float -> [Bullet] -> Point
isHit p s b | mini < s = (-300,300) 
            | otherwise = p
  where
    bulletMap = [pos | Bullet _ pos _ _ <- b]
    m = map (disc s p) bulletMap
    mini = minimum' m

minimum' :: [Float] -> Float
minimum' [] = 99999999999
minimum' [i] = i
minimum' (i:is) | i == (-1) = -1
                | otherwise = min i (minimum' is)
                        
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'p') Down _ _) gstate@(GamePlay {isPaused})
    | isPaused  = gstate {isPaused = False}
    | otherwise = gstate {isPaused = True}

inputKey (EventKey (Char c) Down _ _) gstate@(GamePlay {player = Player vel ang pos})
  = let direction = case c of 
                      'w' -> (0, 1)
                      'a' -> (-1, 0)
                      's' -> (0, -1)
                      'd' -> (1, 0)
                      _   -> (0, 0)
    in
      gstate {player = Player ((vel .+ direction)) ang pos}
        

inputKey (EventKey (MouseButton LeftButton) Down _ clickPos)
  gstate@(GamePlay {player = p@(Player _ _ posp), bullets, isPaused})
    | isPaused  = gstate 
    | otherwise = gstate {bullets = bullets ++ [newBullet]}
    where
      newBullet = Bullet bulletVel posp False ""
      bulletVel =  10 .* norm (clickPos .- posp)

inputKey (EventMotion (x,y)) gstate@(GamePlay {player = (Player velp angle posp@(a,b))}) 
  = gstate {player = Player velp (argV (x-a,y-b)) posp}

inputKey _ gstate = gstate -- Otherwise keep the same


(.+) :: Point -> Point -> Point
(x,y) .+ (a,b) = (x+a, y+b)

(.-) :: Point -> Point -> Point
(x,y) .- (a,b) = (x-a, y-b)

(.*) :: Float -> Vector -> Vector
s .* (x,y) = (s*x,s*y)

dotproduct :: Vector -> Vector -> Float
dotproduct (x,y) (a,b) = x*a + y*b

norm :: Vector -> Vector
norm (x,y) = let m = magn (x,y) in (x/m,y/m)

magn :: Vector -> Float
magn (x,y) = sqrt(x**2 + y**2)

rot :: Point -> Point -> Vector -> Vector
rot p1 p2 vec = res
    where
      dirv = norm (p1 .- p2)
      res = norm (vec .+ (0.05 .* dirv))

disc :: Float -> Point -> Point -> Float
disc size c p = abs(size - (dis c p))

distance :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
distance a b c = dis a c + dis c b > dis a b - 0.5  && dis a c + dis c b < dis a b + 0.5 

dis :: Point -> Point -> Float
dis (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2
