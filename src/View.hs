-- | This module defines how to turn
--   the game state into a picture
module View where

  import Graphics.Gloss
  import Type.State
  import View.Menu
  import View.Playing

  view :: GameState -> IO Picture
  view gs@GameState{mode = m} = case m of
    Menu -> viewMenu gs
    Playing -> viewPlaying gs
    _ -> viewMenu gs