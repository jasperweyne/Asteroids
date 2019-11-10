module Controller.Playing (stepPlaying, eventPlaying) where
  
  import Control.Monad
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
  import IO.Score
  import Game.Player
  import Game.Explosion
  import Game.Rocket
  import Game.Asteroid
  import Game.Saucer
  import Game.Score

  --Split update into pre, object update and post
  stepPlaying :: Float -> GameState -> IO GameState
  stepPlaying t = return . post . step . pre
    where
      pre    = preUpdate  t
      step x = x { inGame = update (inGame x) t }
      post   = postUpdate t

  --Update gamestate for each seperate object type, checks collisions..
  preUpdate :: Float -> GameState -> GameState
  preUpdate t gs@GameState{inGame = igs, processIO = io} = gs{inGame = igs{asteroids = ast2, player = p3, explosions = ex1, pRockets = prs, sRockets = srs, saucers = s, score = sx}, mode = m, processIO = doIO}
      where
        --Update game objects using same gamestate
        p3      = p2 { lives = lives p2 + l }
        p2      = updatePlayer t gs
        ast1    = updateAsteroids t gs
        s       = updateSaucers t gs
        prs     = updatePlayerRockets t gs
        srs     = updateSaucerRockets t gs
        ex1     = updateExplosions t gs
        (sx, l) = updateScoreLives gs
        --Remove asteroids too close to player spawn
        p1      = player igs
        ast2    | lives p2 < lives p1 = removeCloseAsteroids p2 ast1
                | otherwise = ast1
        --If 0 lives, go to score
        m       | lives p2 <= 0 = Score
                | otherwise     = mode gs
        --Save the current scoreboard
        doIO    | m == Score = io >=> saveScoreboard "highscores.txt"
                | otherwise = io

  --Removes asteroid too close to the player
  removeCloseAsteroids :: Player -> [Asteroid] -> [Asteroid]
  removeCloseAsteroids p = mapMaybe
    (\x -> if getGameObject x `collides` (getGameObject p){radius = 50} then
      Nothing else Just x)
  
  --Post update: applies screen wrapping, asteroid spawning..
  postUpdate :: Float -> GameState -> GameState
  postUpdate t = postUpdatePlayer t . postUpdateAsteroids t . postUpdateSaucers t . postUpdateRockets t

  eventPlaying :: Event -> GameState -> IO GameState
  eventPlaying e gs = return $ checkModeSwitch gs 
  
  --Check if player pressed pause
  checkModeSwitch :: GameState -> GameState
  checkModeSwitch gs@GameState{inputState = ks}
    | keyDown ks Pause = gs { mode = Paused }
    | otherwise        = gs