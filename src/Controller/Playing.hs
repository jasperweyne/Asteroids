module Controller.Playing (stepPlaying, eventPlaying) where
  
  import Data.Maybe
  import Class.Updateable
  import Graphics.Gloss.Interface.IO.Game
  import Class.HasGameObject
  import Type.State
  import Type.IO.Input
  import Type.Object.Player
  import Type.Object.Asteroid
  import Type.Object.Explosion
  import Type.Physics.GameObject
  import Physics.Collisions
  import Game.Player
  import Game.Explosion
  import Game.Rocket
  import Game.Asteroid
  import Game.Saucer
  import Game.Score

  stepPlaying :: Float -> GameState -> IO GameState
  stepPlaying t = return . post . step . pre
    where
      pre    = preUpdate  t
      step x = x { inGame = update (inGame x) t }
      post   = postUpdate t

  preUpdate :: Float -> GameState -> GameState
  preUpdate t gs@GameState{inGame = igs} = gs{inGame = igs{asteroids = ast2, player = p2, explosions = ex1, pRockets = prs, sRockets = srs, saucers = s, score = sx}, mode = m}
      where
        --Update game objects using same gamestate
        p2   = updatePlayer t gs
        ast1 = updateAsteroids t gs
        s    = updateSaucers t gs
        prs  = updatePlayerRockets t gs
        srs  = updateSaucerRockets t gs
        p1   = player igs
        ex1  = updateExplosions t gs
        sx   = updateScore gs
        --Remove asteroids too close to player spawn
        ast2 | lives p2 < lives p1 = removeCloseAsteroids p2 ast1
             | otherwise = ast1
        --If 0 lives, go to score
        m | lives p2 <= 0 = Score
          | otherwise     = mode gs

  removeCloseAsteroids :: Player -> [Asteroid] -> [Asteroid]
  removeCloseAsteroids p = mapMaybe
    (\x -> if getGameObject x `collides` (getGameObject p){radius = 50} then
      Nothing else Just x)
  
  postUpdate :: Float -> GameState -> GameState
  postUpdate t = postUpdatePlayer t . postUpdateAsteroids t . postUpdateSaucers t . postUpdateRockets t

  eventPlaying :: Event -> GameState -> IO GameState
  eventPlaying e gs = return $ checkModeSwitch gs 
  
  checkModeSwitch :: GameState -> GameState
  checkModeSwitch gs@GameState{inputState = ks}
    | keyDown ks Pause = gs { mode = Paused }
    | otherwise        = gs