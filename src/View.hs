module View where

  import Graphics.Gloss
  import Type.State
  import View.Menu
  import View.Playing
  import View.Paused
  import View.Score

  view :: GameState -> IO Picture
  view gs@GameState{mode = m} = case m of
    Menu    -> viewMenu gs
    Playing -> viewPlaying gs
    Paused  -> viewPaused gs
    Score   -> viewScore gs