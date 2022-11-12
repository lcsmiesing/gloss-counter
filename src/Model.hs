module Model where

import Graphics.Gloss.Data.Vector ( Vector )
import Graphics.Gloss.Data.Point.Arithmetic ( Point )

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 0


data GameState = GamePlay {
                   player :: Player
                 , asteroids  :: [Asteroid]
                 , elapsedTime :: Float
                 , enemies :: [Enemy]
                 , bullets :: [Bullet]
                 , obstacles :: [Obstacle]
                 , points :: Int
                 , isPaused :: Bool
                 , gameBorders :: (Point,Point)
                 , animations :: [Animation]
                 }
                | GameOver {
                  points :: Int
                }
data Player = Player {
                velp :: Vector
              , angle :: Float
              , posp :: Point
              }
data Asteroid = Asteroid {
                  vela :: Vector
                , posa :: Point
                , size :: Float
                , lastBounce :: String
                }


data Enemy = Enemy {
              vele :: Vector
            , pose :: Point
            , lastBounceE :: String
            }

data Bullet = Bullet {
                velb :: Vector
              , posb :: Point
              , fromEnemy :: Bool
              , lastBounceB :: String
              }

data Obstacle = Obstacle {
                  poso :: Point
                , width :: Float
                , height :: Float
                , boundingBox :: (Point,Point,Point,Point)
                }

data Animation = Animation {
                  position :: Point
                , time :: Float
                , typ :: String
                } deriving (Eq)

data Line = Line Point Point

initialState :: [String] -> [Float] -> [Float] -> [Float] -> [Float] -> Point -> GameState
initialState s x y v w (s1,s2) = GamePlay {
                   player = Player (0,0) 0 (0,0)
                 , asteroids = map crAs zipo
                 , elapsedTime = 0
                 , enemies = map crEm zipvw
                 , bullets = []
                 , obstacles = obs
                 , points = 0
                 , isPaused = False
                 , gameBorders = ((-s1/2,s2/2),(-s1/2,s2/2))
                 , animations = []
                 }
               where
                  obs = map (getObs . readStr) s
                  zipxy = zip x y
                  zipvw = zip v w
                  zipo = zip zipxy zipvw

readStr :: String -> [Float]
readStr s = map read $ words s

crAs :: ((Float,Float),(Float,Float)) -> Asteroid
crAs ((x,y),(v,w)) = Asteroid (x,y) (300,0) 15 ""

crEm :: ((Float,Float)) -> Enemy
crEm (x,y) = Enemy (x,y) (-300,300) ""

getBoundingBox :: (Float,Float,Float,Float) -> (Point,Point,Point,Point)
getBoundingBox (x,y,w,h) =  ((x-w/2,y+h/2),(x+w/2,y+h/2),(x-w/2,y-h/2),(x+w/2,y-h/2))
-- ((-200,-25),(200, -25),(-200,-225),(200,-225))

getObs :: [Float] -> Obstacle
getObs (a:b:c:d:_) = Obstacle (a,b) c d (getBoundingBox (a,b,c,d))
