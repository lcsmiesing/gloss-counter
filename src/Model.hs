-- | This module contains the data types
--   which represent the state of the game
module Model where

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   asteroids  :: [Asteroid]
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
                velocity :: Vector
              , angle :: Vector
              , position :: Point
              }
data Asteroid = Asteroid {
                  velocity :: Vector
                , position :: Point
                , size :: Int
                }
data Enemy = Enemy {
              velocity :: Vector
            , position :: Point
            }
data Bullet = Bullet {
                velocity :: Vector
              , position :: Point
              , fromEnemy :: Bool
              }
data Obstacle = Obstacle {
                  position :: (Point, Point)
                , width :: Int
                }

data Point = (Float, Float)
data Vector = (Float, Float)



initialState :: GameState
initialState = GameState ShowNothing 0