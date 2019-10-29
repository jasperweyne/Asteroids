module Controller.Menu (stepMenu, eventMenu) where
  import Graphics.Gloss.Interface.IO.Game
  import IO.Queue
  import IO.Picture
  import Type.State

  stepMenu :: Float -> GameState -> IO GameState
  stepMenu t = return

  eventMenu :: Event -> GameState -> IO GameState
  eventMenu e gstate = return (inputKey e gstate)
  
  inputKey :: Event -> GameState -> GameState
  inputKey (EventKey (SpecialKey c) _ _ _) gstate
    = case c of -- If the user presses a character key, show that one
      KeyEnter -> queueIO (gstate { mode = Playing }) loadPlayerPicture
      _ -> gstate
  inputKey _ gstate = gstate -- Otherwise keep the same