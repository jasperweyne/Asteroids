module Controller.Paused (stepPaused, eventPaused) where

  import Graphics.Gloss.Interface.IO.Game
  import Type.State
  import Type.IO.Input

  stepPaused :: Float -> GameState -> IO GameState
  stepPaused t = return

  eventPaused :: Event -> GameState -> IO GameState
  eventPaused e gs = return (checkModeSwitch gs)
  
  checkModeSwitch :: GameState -> GameState
  checkModeSwitch gs@GameState{inputState = ks}
    | keyDown ks Pause = gs{mode = Playing}
    | otherwise        = gs