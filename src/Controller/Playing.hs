module Controller.Playing (stepPlaying, eventPlaying) where
  import Class.Updateable
  import Graphics.Gloss.Interface.IO.Game
  import Type.State
  import Type.IO.Input
  import Game.Player

  stepPlaying :: Float -> GameState -> IO GameState
  stepPlaying t gs = return gs {
    inGame = post . flip update t $ pre
  }
    where
      pre    = preUpdate  t  gs
      post x = postUpdate t (gs { inGame = x })

  preUpdate :: Float -> GameState -> InGameState
  preUpdate t gs = inGame $ updatePlayer t gs
  
  postUpdate :: Float -> GameState -> InGameState
  postUpdate t gs = inGame $ postUpdatePlayer t gs

  eventPlaying :: Event -> GameState -> IO GameState
  eventPlaying e gs = return $ checkModeSwitch gs 
  
  checkModeSwitch :: GameState -> GameState
  checkModeSwitch gs@GameState{inputState = inState} = case (keyDown inState Pause) of
    True -> gs { mode = Menu }
    _ -> gs