module Controller.Playing (stepPlaying, eventPlaying) where
  import Class.Updateable
  import Graphics.Gloss.Interface.IO.Game
  import Type.State
  import Type.IO.Input
  import Game.Player

  stepPlaying :: Float -> GameState -> IO GameState
  stepPlaying t gs = return $ updatePlayer t gs

  eventPlaying :: Event -> GameState -> IO GameState
  eventPlaying e gs = return $ checkModeSwitch gs 
  
  checkModeSwitch :: GameState -> GameState
  checkModeSwitch gs@GameState{inputState = inState} = case (keyDown inState Pause) of
    True -> gs { mode = Menu }
    _ -> gs