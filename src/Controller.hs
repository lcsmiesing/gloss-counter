{-# language NamedFieldPuns #-}

-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do return $ gstate
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  = case c of
      'w' -> gstate
      'a' -> gstate
      's' -> gstate
      'd' -> gstate

inputKey (EventKey (MouseButton LeftButton) Down _ mousePos)
  gstate@(GamePlay {player = p@(Player _ _ posp), bullets}) = gstate {bullets = bullets ++ [newBullet]}
    where
      newBullet = Bullet bulletVel posp False
      bulletVel =  norm (posp .- mousePos)
inputKey _ gstate = gstate -- Otherwise keep the same

(.+) :: Point -> Point -> Point
(.+) (x,y) (a,b) = (x+a, y+b)

(.-) :: Point -> Point -> Point
(.-) (x,y) (a,b) = (x-a, y-b)

(.*) :: Vector -> Float -> Vector
(.*) (x,y) s = (s*x,s*y)

norm :: Vector -> Vector
norm (x,y) = let m = magn (x,y) in (x/m,y/m)

magn :: Vector -> Float
magn (x,y) = sqrt(x**2 + y**2)
