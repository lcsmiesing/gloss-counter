-- | This module contains the data types
--   which represent the state of the game
module Model where

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5


data GameState = GameState {
                   player :: Player
                 , asteroids  :: [Asteroid]
                 , elapsedTime :: Float
                 , enemies :: [Enemy]
                 , bullets :: [Bullet]
                 , obstacles :: [Obstacle]
                 , points :: Int
                 }
                | GameOver {
                  points :: Int
                }
data Player = Player {
                velp :: Vector
              , angle :: Vector
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



data Point = Point Float Float 
data Vector = Vector Float Float



initialState :: GameState
initialState = GameState ShowNothing 0