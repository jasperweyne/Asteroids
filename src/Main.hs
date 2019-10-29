module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Controller
import Model
import View

main :: IO ()
main = do
    let screen = (1024, 768)
    playIO (InWindow "Game" screen (0, 0)) -- Or FullScreen
              black            -- Background color
              30               -- Frames per second
              initialGameState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function