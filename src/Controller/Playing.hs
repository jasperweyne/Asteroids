module Controller.Playing (stepPlaying, eventPlaying) where
  import Graphics.Gloss.Interface.IO.Game
  import Type.State
  import Type.IO.Input

  stepPlaying :: Float -> GameState -> IO GameState
  stepPlaying t = return

  eventPlaying :: Event -> GameState -> IO GameState
  eventPlaying e gstate = return (checkModeSwitch gstate)
  
  checkModeSwitch :: GameState -> GameState
  checkModeSwitch gs@GameState{inputState = inState} = case (keyDown inState Pause) of
    True -> gs { mode = Menu }
    _ -> gs