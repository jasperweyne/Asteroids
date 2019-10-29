module IO.Picture where

  import Graphics.Gloss
  import Graphics.Gloss.Juicy
  import Rendering.GameObject
  import Type.Object.Player
  import Type.State

  loadPlayerPicture :: GameState -> IO GameState
  loadPlayerPicture gs@GameState{ inGame = igs@InGameState { player = p }} = 
    do
      maybeImg <- loadJuicy "img/rocket_still.png"
      case maybeImg of
        Just img -> return gs { inGame = igs { player = p { picture = img }}}
        Nothing  -> return gs
      

  loadAsteroidPicture :: GameState -> IO GameState
  loadAsteroidPicture = undefined