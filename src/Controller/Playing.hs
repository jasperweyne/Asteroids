module Controller.Playing (stepPlaying, eventPlaying) where
  import Class.Updateable
  import Graphics.Gloss.Interface.IO.Game
  import Type.State
  import Type.IO.Input
  import Game.Player
  import Game.Rocket
  import Game.Asteroid
  import Game.Saucer

  stepPlaying :: Float -> GameState -> IO GameState
  stepPlaying t = return . post . step . pre
    where
      pre  = preUpdate  t
      step x = x { inGame = update (inGame x) t }
      post = postUpdate t

  preUpdate :: Float -> GameState -> GameState
  preUpdate t gs@GameState{inGame = igs} = gs{inGame = igs{asteroids = ast, player = p, rockets = rs, saucers = s}}
      where
        p = updatePlayer t gs
        ast = updateAsteroids t gs
        s = updateSaucers t gs
        rs = updateRockets t gs
  
  postUpdate :: Float -> GameState -> GameState
  postUpdate t = postUpdatePlayer t . postUpdateAsteroids t . postUpdateSaucers t . postUpdateRockets t

  eventPlaying :: Event -> GameState -> IO GameState
  eventPlaying e gs = return $ checkModeSwitch gs 
  
  checkModeSwitch :: GameState -> GameState
  checkModeSwitch gs@GameState{inputState = inState}
    | keyDown inState Pause = gs { mode = Menu }
    | otherwise             = gs