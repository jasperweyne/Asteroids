module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

--import Controller
import Model
--import View
import Controller.Menu
import View.Menu


main :: IO ()
main = do
    let screen = (1024, 768)
    playIO (InWindow "Game" screen (0, 0)) -- Or FullScreen
              black            -- Background color
              30               -- Frames per second
              initialGameState     -- Initial state
              viewMenu             -- View function
              eventMenu            -- Event function
              stepMenu             -- Step function