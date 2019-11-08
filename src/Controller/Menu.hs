module Controller.Menu (stepMenu, eventMenu) where
  import Control.Monad
  import Graphics.Gloss.Interface.IO.Game
  import IO.Queue
  import IO.Picture
  import Type.State
  import Type.IO.Input

  stepMenu :: Float -> GameState -> IO GameState
  stepMenu t = return

  eventMenu :: Event -> GameState -> IO GameState
  eventMenu e gstate = return (checkModeSwitch gstate)
  
  checkModeSwitch :: GameState -> GameState
  checkModeSwitch gs@GameState{inputState = inState}
    | keyDown inState Start = queueIO (gs { mode = Playing }) (loadPlayerPicture >=> loadPlayerAnim >=> loadAsteroidPicture >=> loadSaucerPicture)
    | otherwise = gs
