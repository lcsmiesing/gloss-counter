-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

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



initialState :: GameState
initialState = GameState ShowNothing 0