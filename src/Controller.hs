{-# language NamedFieldPuns #-}

-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss.Data.Vector
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

runGame gstate@(GamePlay {bullets = b}) = gstate {bullets = map updateBull b}
  where
    updateBull (Bullet vel pos f) = Bullet vel (vel .+ pos) f

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GamePlay {isPaused = p})
  | not p
  = -- We show a new random number
    do return $ runGame gstate
  | p
  =
    do return gstate
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'p') Down _ _) gstate@(GamePlay {isPaused})
    | isPaused        = gstate {isPaused = False}
    | otherwise       = gstate {isPaused = True}

inputKey (EventKey (Char c) Down _ _) gstate
  = case c of 
      'w' -> gstate
      'a' -> gstate
      's' -> gstate
      'd' -> gstate
      _   -> gstate

inputKey (EventKey (MouseButton LeftButton) Down _ clickPos)
  gstate@(GamePlay {player = p@(Player _ _ posp), bullets, isPaused})
    | isPaused  = gstate 
    | otherwise = gstate {bullets = bullets ++ [newBullet]}
    where
      newBullet = Bullet bulletVel posp False
      bulletVel =  10 .* norm (clickPos .- posp)

inputKey (EventMotion (x,y)) gstate@(GamePlay {player = (Player velp a posp)}) = gstate {player = Player velp (argV (x,y)) posp}

inputKey _ gstate = gstate -- Otherwise keep the same

(.+) :: Point -> Point -> Point
(x,y) .+ (a,b) = (x+a, y+b)

(.-) :: Point -> Point -> Point
(x,y) .- (a,b) = (x-a, y-b)

(.*) :: Float -> Vector -> Vector
s .* (x,y) = (s*x,s*y)

norm :: Vector -> Vector
norm (x,y) = let m = magn (x,y) in (x/m,y/m)

magn :: Vector -> Float
magn (x,y) = sqrt(x**2 + y**2)
