module Main where

import Graphics.Gloss.Interface.IO.Game (Display(..), playIO)
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Game hiding (play, Event(..))

main :: IO ()
main = do
    let screen = (1024, 768)
    {-
    playIO (InWindow "Game" screen (0, 0)) -- Screen
            black                          -- Background color
            30                             -- Frames per second
            ?                              -- Model
            ?                              -- View method
            ?                              -- Event method
            ?                              -- Step method
            -}