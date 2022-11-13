module Model where

import Graphics.Gloss.Data.Vector ( Vector )
import Graphics.Gloss.Data.Point.Arithmetic ( Point )

--Data custom datatypes used in the game
data GameState = GamePlay {
                   player :: Player
                 , asteroids  :: [Asteroid]
                 , elapsedTime :: Float
                 , enemies :: [Enemy]
                 , bullets :: [Bullet]
                 , obstacles :: [Obstacle]
                 , points :: Float
                 , isPaused :: Bool
                 , gameBorders :: (Point,Point)
                 , animations :: [Animation]
                 }
                | GameOver {
                  points :: Float
                }
data Player = Player {
                velp :: Vector --velocity vector
              , angle :: Float --angle
              , posp :: Point --position of player
              , lives :: Float --number of lives
              }
data Asteroid = Asteroid {
                  vela :: Vector --velocity vector
                , posa :: Point --position of asteroid
                , size :: Float --size of the asteroid
                }


data Enemy = Enemy {
              vele :: Vector--velocity vector
            , pose :: Point --position of the enemy
            }

data Bullet = Bullet {
                velb :: Vector--velocity vector
              , posb :: Point -- position of bullet
              , fromEnemy :: Bool --determines if it shot from an enemy
              }

data Obstacle = Obstacle {
                  poso :: Point --centre of obstacle
                , width :: Float -- width of obstacle
                , height :: Float -- height of obstacle
                }

data Animation = Animation {
                  position :: Point --position of animation
                , time :: Float --timer for frames of anumation
                , typ :: String
                } deriving (Eq)

--Initial state of the world  using information from the "obs.txt" file
initialState :: [String] -> [Float] -> [Float] -> [Float] -> [Float] -> Point -> GameState
initialState s x y v w (s1,s2) = GamePlay {
                   player = Player (0,0) 0 (0,0) 150
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

--Make floats out of string
readStr :: String -> [Float]
readStr s = map read $ words s

--Creation of asteroids
crAs :: ((Float,Float),(Float,Float)) -> Asteroid
crAs ((x,y),(v,w)) = Asteroid (x,y) (300,0) 25

--Creation of enemies
crEm :: ((Float,Float)) -> Enemy
crEm (x,y) = Enemy (x,y) (-300,300)

--Creation of enemies
getObs :: [Float] -> Obstacle
getObs (a:b:c:d:_) = Obstacle (a,b) c d
