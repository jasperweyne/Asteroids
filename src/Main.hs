module Main where

import Graphics.Gloss.Interface.IO.Game (Display(..), playIO)
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Game hiding (play, Event(..))

import Controller
import Model
import View

main :: IO ()
main = do
    let screen = (1024, 768)
    playIO (InWindow "Game" screen (0, 0)) -- Or FullScreen
              black            -- Background color
              30               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function