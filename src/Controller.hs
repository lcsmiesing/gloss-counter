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
                          elapsedTime = e,
                          animations = anims}) = gstate {player = updatePlayer p, bullets = map updateBull b, asteroids = newas, enemies = map updateEnemies em, animations = newaniii}
  where
    
    get as@(Asteroid vel pos f s) = diff2 (vel,s,pos, "") (map (coll as) ob)
    gete em@(Enemy vel pos s) = diff (vel,s) (map (colle em) ob)
    getb b@(Bullet vel pos d s) =  diff (vel,s) (map (collb b) ob)
    updates = map get a
    omg = map (\(x,y,z,zz) -> (z,zz)) updates
    nonut = [dm | dm@(z,zz) <- omg, zz == "hit"]
    newanim | nonut /= [] = map (\(x,y) -> Animation x 360 "") nonut
            | otherwise = []
    newaniii = map (\(Animation x y z) -> (Animation x (y-1) z)) (anims ++ newanim)
    newas = map (\((x,y,z,zz),(Asteroid vel pos f s)) -> Asteroid x (isHit (edgeDetection gstate (vel .+ pos)) f b) f y) (zip updates a)
    updateEnemies em@(Enemy vel pos s) = Enemy (rot ppos pos (fst (gete em))) (edgeDetection gstate(vel .+ pos)) (snd(gete em))
    updateBull bu@(Bullet vel pos f d) = Bullet (fst (getb bu)) (vel .+ pos) f d
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
 
coll :: Asteroid -> Obstacle -> (Vector,String,Point,String)
coll (Asteroid v p@(x,y) _ f) (Obstacle _ _ _ (a,b,c,d)) 
                                                       | top && f /= "top"  = (v .- ((2*dotproduct v (0,1)).*(0,1)),"top",p,"") 
                                                       | left && f/= "left"= (v .- ((2*dotproduct v (-1,0)).*(-1,0)),"left",p,"")
                                                       | right && f/="right"= (v .- ((2*dotproduct v (1,0)).*(1,0)),"right",p,"")
                                                       | bottom && f/="bottom"= (v .- ((2*dotproduct v (0,-1)).*(0,-1)),"bottom",p,"")
                                                       | otherwise = (v,f,p,"")
  where
    top = distance a b p -- (0,1)
    left = distance a c p --(-1,0)
    right = distance b d p --(1,0)
    bottom = distance c d p --(0,-1)

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

collb :: Bullet -> Obstacle -> (Vector,String)
collb (Bullet v p t f) (Obstacle _ _ _ (a,b,c,d)) | top && f /= "top" = (v .- ((2*dotproduct v (0,1)).*(0,1)),"top")
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

diff2 :: (Vector,String,Point,String) -> [(Vector,String,Point,String)] -> (Vector,String,Point,String)
diff2 v [] = v
diff2 cc@(v,vv,vvv,vvvv) (r@(x,xx,xxx,xxxx):xs) | v /= x = (x,xx,xxx,"hit")
                                | otherwise = diff2 cc xs

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
minimum' (i:is) = min i (minimum' is)
                        
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'p') Down _ _) gstate@(GamePlay {isPaused})
    | isPaused  = gstate {isPaused = False}
    | otherwise = gstate {isPaused = True}

inputKey (EventKey (Char c) Down _ _) gstate@(GamePlay {player = Player vel ang pos})
  = let direction = case c of 
                      'w' -> (0, 5)
                      'a' -> (-5, 0)
                      's' -> (0, -5)
                      'd' -> (5, 0)
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
