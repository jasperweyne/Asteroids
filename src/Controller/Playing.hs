module Controller.Playing (stepPlaying, eventPlaying) where
  import Graphics.Gloss.Interface.IO.Game
  import Type.State

  stepPlaying :: Float -> GameState -> IO GameState
  stepPlaying t = return

  eventPlaying :: Event -> GameState -> IO GameState
  eventPlaying e gstate = return (inputKey e gstate)
  
  inputKey :: Event -> GameState -> GameState
  inputKey (EventKey (Char c) _ _ _) gstate
    = case c of -- If the user presses a character key, show that one
      's' -> gstate { mode = Menu }
      _ -> gstate
  inputKey _ gstate = gstate -- Otherwise keep the same