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
    do randomNumber <- randomIO
       let newNumber = abs randomNumber `mod` 10
       return $ GameState (ShowANumber newNumber) 0
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  = case c of
      'w' -> --iets jwz
      'a' ->
      's' ->
      'd' ->

inputKey (EventKey (MouseButton LeftButton) Down MousePos)
  = gstate@(GameState {player, bullets}) = gstate {bullets = bullets:newBullet}
    where
      newBullet = Bullet {velb = bulletVel posb, posb = player@posp, fromEnemy = False}
      bulletVel p =  norm (p .- MousePos) .* 10
      posb = --SDFHSDFSDKLFSLDJFLKSDJFSLKDJFSLKDFSDLKJF
inputKey _ gstate = gstate -- Otherwise keep the same

.+ :: Point -> Point -> Point
.+ (Point x y) (Point a b) = Point (x+a) (y+b)

.- :: Point -> Point -> Point
.- (Point x y) (Point a b) = Point (x-a) (y-b)

.* :: Vector -> Float -> Vector
.* (Vector x y) s = Vector (s*x) (s*y)

norm :: Vector -> Vector
norm v@(Vector x y) = let m = magn v in (Vector x/m y/m)

magn :: Vector -> Float
magn (Vector x y) = sqrt(x**2 + y**2)
