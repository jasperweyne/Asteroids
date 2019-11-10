module Controller.Menu (stepMenu, eventMenu) where
  
  import Control.Monad
  import Graphics.Gloss.Interface.IO.Game
  import IO.Queue
  import IO.Picture
  import IO.Random
  import IO.Score
  import Type.State
  import Type.IO.Input
  import Game.Asteroid

  --Empty for menu
  stepMenu :: Float -> GameState -> IO GameState
  stepMenu t = return

  eventMenu :: Event -> GameState -> IO GameState
  eventMenu e gs = return (checkModeSwitch gs)
  
  --Check if player pressed continue, load resources
  checkModeSwitch :: GameState -> GameState
  checkModeSwitch gs@GameState{inputState = ks}
    | keyDown ks Start = queueIO (gs { mode = Playing }) 
      (loadPlayerPicture >=> 
        loadPlayerAnim >=> 
          loadRocketPicture >=> 
            loadAsteroidPicture >=>
              loadSaucerPicture >=> 
                loadExplosionAnim >=>
                  loadScoreboard "highscores.txt" >=>
                    generateStdRandom >=>
                      (return . attemptAsteroidSpawns 10 . attemptAsteroidSpawns 10 . attemptAsteroidSpawns 10))
    | otherwise = gs
