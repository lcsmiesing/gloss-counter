module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
    m <- readFile "obs.txt"
    let l = lines m
    playIO (InWindow "Best game 4ever" (600, 600) (0, 0)) -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              (initialState l)     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function