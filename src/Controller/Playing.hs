module Controller.Playing (stepPlaying, eventPlaying) where
  import Class.Updateable
  import Graphics.Gloss.Interface.IO.Game
  import Class.HasGameObject
  import Type.State
  import Type.IO.Input
  import Type.Object.Player
  import Type.Object.Explosion
  import Type.Physics.GameObject
  import Game.Player
  import Game.Explosion
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
  preUpdate t gs@GameState{inGame = igs} = gs{inGame = igs{asteroids = ast, player = p2, explosions = ex2, rockets = rs, saucers = s}}
      where
        p2 = updatePlayer t gs
        ast = updateAsteroids t gs
        s = updateSaucers t gs
        rs = updateRockets t gs
        p1 = player igs
        ex1 = updateExplosions t gs
        ex2 | lives p2 < lives p1 = makeExplosion ((pos.getGameObject) p1) (explosion gs) : ex1 
            | otherwise = ex1
  
  postUpdate :: Float -> GameState -> GameState
  postUpdate t = postUpdatePlayer t . postUpdateAsteroids t . postUpdateSaucers t . postUpdateRockets t

  eventPlaying :: Event -> GameState -> IO GameState
  eventPlaying e gs = return $ checkModeSwitch gs 
  
  checkModeSwitch :: GameState -> GameState
  checkModeSwitch gs@GameState{inputState = inState}
    | keyDown inState Pause = gs { mode = Menu }
    | otherwise             = gs