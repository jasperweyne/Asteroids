module Controller.Menu (stepMenu, eventMenu) where
  import Graphics.Gloss.Interface.IO.Game
  import Type.State

  stepMenu :: Float -> GameState -> IO GameState
  stepMenu t = return

  eventMenu :: Event -> GameState -> IO GameState
  eventMenu e gstate = return (inputKey e gstate)
  
  inputKey :: Event -> GameState -> GameState
  inputKey (EventKey (Char c) _ _ _) gstate
    = -- If the user presses a character key, show that one
      gstate
  inputKey _ gstate = gstate -- Otherwise keep the same