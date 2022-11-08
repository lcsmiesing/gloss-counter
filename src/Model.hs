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
                  poso :: (Point, Point)
                , width :: Int
                }

initialState :: GameState
initialState = GamePlay {
                   player = Player (0,0) 0 (0,50)
                 , asteroids  = []
                 , elapsedTime = 0
                 , enemies = []
                 , bullets = []
                 , obstacles = []
                 , points = 0
                 , isPaused = False
                 }