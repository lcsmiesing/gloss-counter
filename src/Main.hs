module Main where

import Controller
import Model
import View
import Graphics.Gloss.Interface.IO.Game
import System.Random

--Variables  from the "obs.txt" file are imported and read, the data is passed on to the initialState function
main :: IO () 
main = do
    let noOfObjects = 6
    let size = (600,600)
    m <- readFile "obs.txt"
    x <- randomList noOfObjects
    y <- randomList noOfObjects
    v <- randomList noOfObjects
    w <- randomList noOfObjects
    let l = lines m
    playIO (InWindow "Best game 4ever" size (0, 0)) -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              (initialState l x y v w (600,600))     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function


--Random data float generation
randomList :: Int -> IO [Float]
randomList 0 = return []
randomList n = do
  r  <- randomRIO(-1,1) :: IO Float
  rs <- randomList (n-1)
  return (r:rs) 
