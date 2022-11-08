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
                , size :: Int
                }
data Enemy = Enemy {
              vele :: Vector
            , pose :: Point
            }

data Bullet = Bullet {
                velb :: Vector
              , posb :: Point
              , fromEnemy :: Bool
              }

data Obstacle = Obstacle {
                  poso :: Point
                , width :: Float
                , height :: Float
                }

initialState :: [String] -> GameState
initialState s = GamePlay {
                   player = Player (0,0) 0 (0,0)
                 , asteroids  = []
                 , elapsedTime = 0
                 , enemies = []
                 , bullets = []
                 , obstacles = obs
                 , points = 0
                 , isPaused = False
                 }
               where
                  obs = map (getObs . readStr) s

readStr :: String -> [Float]
readStr s = map read $ words s

getObs :: [Float] -> Obstacle
getObs (a:b:c:d:_) = Obstacle (a,b) c d