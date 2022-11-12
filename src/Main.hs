module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import System.Random

main :: IO ()
main = do
    let aran = 6
    let size = (600,600)
    m <- readFile "obs.txt"
    x <- randomList aran
    y <- randomList aran
    v <- randomList aran
    w <- randomList aran
    let l = lines m
    playIO (InWindow "Best game 4ever" size (0, 0)) -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              (initialState l x y v w (600,600))     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function



randomList :: Int -> IO [Float]
randomList 0 = return []
randomList n = do
  r  <- randomRIO(-1,1) :: IO Float
  rs <- randomList (n-1)
  return (r:rs) 
