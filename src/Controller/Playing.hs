module Controller.Playing (stepPlaying, eventPlaying) where
  import Class.Updateable
  import Graphics.Gloss.Interface.IO.Game
  import Type.State
  import Type.IO.Input
  import Game.Player
  import Game.Asteroid
  import Game.Saucer

  stepPlaying :: Float -> GameState -> IO GameState
  stepPlaying t = return . post . step . pre
    where
      pre  x = preUpdate  t x
      step x = x { inGame = update (inGame x) t }
      post x = postUpdate t x

  preUpdate :: Float -> GameState -> InGameState
  preUpdate t gs@GameState{inGame = igs} = igs{asteroids = ast, player = p}
      where
        p = updatePlayer t gs
        ast = updateAsteroids t gs
  
  postUpdate :: Float -> GameState -> GameState
  postUpdate t gs = postUpdatePlayer t . postUpdateAsteroids t . postUpdateSaucers t $ gs

  eventPlaying :: Event -> GameState -> IO GameState
  eventPlaying e gs = return $ checkModeSwitch gs 
  
  checkModeSwitch :: GameState -> GameState
  checkModeSwitch gs@GameState{inputState = inState}
    | keyDown inState Pause = gs { mode = Menu }
    | otherwise             = gs