module View where

  import Graphics.Gloss
  import Type.State
  import View.Menu
  import View.Playing
  import View.Paused
  import View.Score
  import View.Settings

  --Switch view function depending on current mode
  view :: GameState -> IO Picture
  view gs@GameState{mode = m} = case m of
    Menu     -> viewMenu gs
    Playing  -> viewPlaying gs
    Paused   -> viewPaused gs
    Score    -> viewScore gs
    Settings -> viewSettings gs
