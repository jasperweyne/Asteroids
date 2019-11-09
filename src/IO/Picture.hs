module IO.Picture (loadPlayerPicture, loadPlayerAnim, loadExplosionAnim, loadRocketPicture, loadAsteroidPicture, loadSaucerPicture) where

  import Data.Maybe
  import Graphics.Gloss
  import Graphics.Gloss.Juicy
  import Rendering.GameObject
  import Type.Object.Player
  import Type.Rendering.Animation
  import Type.State

  loadPlayerPicture :: GameState -> IO GameState
  loadPlayerPicture gs@GameState{ inGame = igs@InGameState { player = p }} = 
    do
      maybeImg <- loadJuicy "img/rocket_still.png"
      case maybeImg of
        Just img -> return gs { inGame = igs { player = p { picture = img }}}
        Nothing  -> return gs

  loadPlayerAnim :: GameState -> IO GameState
  loadPlayerAnim gs@GameState{ inGame = igs@InGameState { player = p }} = 
    do
      maybeAnim1 <- loadJuicy "img/rocket_anim1.png"
      maybeAnim2 <- loadJuicy "img/rocket_anim2.png"
      return $ case buildGameState maybeAnim1 maybeAnim2 of 
        (Just newGs) -> newGs
        Nothing    -> gs
    where
      buildGameState maybeAnim1 maybeAnim2 = do
        anim1 <- maybeAnim1
        anim2 <- maybeAnim2
        return gs { inGame = igs { player = p {
          moving = Animation {
            frames = [anim1, anim2],
            frametime = 0.3,
            currenttime = 0
          }
        }}}

  loadExplosionAnim :: GameState -> IO GameState
  loadExplosionAnim gs = 
    do 
      maybeAnims <- mapM (\x -> loadJuicy ("img/explosion/explosion-" ++ x ++ ".png")) ((\x -> if x < 10 then "0" ++ show x else show x) <$> [0..15])
      return $ buildGameState maybeAnims
    where
      buildGameState ma = gs { 
          explosion = Animation {
            frames = catMaybes ma,
            frametime = 0.1,
            currenttime = 0
          }
        }


  loadRocketPicture :: GameState -> IO GameState
  loadRocketPicture gs = 
    do
      maybeImg <- loadJuicy "img/projectile.png"
      case maybeImg of
        Just img -> return gs { rocketPicture = img }
        Nothing  -> return gs


  loadAsteroidPicture :: GameState -> IO GameState
  loadAsteroidPicture gs = 
    do
      maybeImg <- loadJuicy "img/asteroid.png"
      case maybeImg of
        Just img -> return gs { asteroidPicture = Scale 0.1 0.1 img }
        Nothing  -> return gs
        
  loadSaucerPicture :: GameState -> IO GameState
  loadSaucerPicture gs = 
    do
      maybeImg <- loadJuicy "img/saucer.png"
      case maybeImg of
        Just img -> return gs { saucerPicture = img }
        Nothing  -> return gs 