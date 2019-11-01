module IO.Picture where

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
      return $ case (buildGameState maybeAnim1 maybeAnim2) of 
        Just newGs -> newGs
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
      

  loadAsteroidPicture :: GameState -> IO GameState
  loadAsteroidPicture = undefined