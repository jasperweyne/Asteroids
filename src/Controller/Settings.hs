module Controller.Settings (stepSettings, eventSettings) where
  
  import Graphics.Gloss.Interface.IO.Game
  import Type.State
  import Type.IO.Input

  stepSettings :: Float -> GameState -> IO GameState
  stepSettings t = return

  eventSettings :: Event -> GameState -> IO GameState
  eventSettings e gs = return (checkModeSwitch gs)
  
  checkModeSwitch :: GameState -> GameState
  checkModeSwitch gs@GameState{inputState = ks@InputState{ inputmode = m}}
    | keyDown ks Start = gs { inputState = ks {
        inputmode = if m == Keyboard then Mouse else Keyboard,
        keys = [GameKeyState gks Up | gks <- enumFrom Forward]
      } }
    | keyDown ks Pause = gs { mode = Menu }
    | otherwise = gs
