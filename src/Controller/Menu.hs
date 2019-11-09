module Controller.Menu (stepMenu, eventMenu) where
  
  import Control.Monad
  import Graphics.Gloss.Interface.IO.Game
  import IO.Queue
  import IO.Picture
  import IO.Random
  import Type.State
  import Type.IO.Input
  import Game.Asteroid

  stepMenu :: Float -> GameState -> IO GameState
  stepMenu t = return

  eventMenu :: Event -> GameState -> IO GameState
  eventMenu e gs = return (checkModeSwitch gs)
  
  checkModeSwitch :: GameState -> GameState
  checkModeSwitch gs@GameState{inputState = ks}
    | keyDown ks Start = queueIO (gs { mode = Playing }) 
      (loadPlayerPicture >=> 
        loadPlayerAnim >=> 
          loadRocketPicture >=> 
            loadAsteroidPicture >=>
              loadSaucerPicture >=> 
                loadExplosionAnim >=>
                  generateStdRandom >=>
                    (return . attemptAsteroidSpawns 10 . attemptAsteroidSpawns 10 . attemptAsteroidSpawns 10))
    | otherwise = gs
