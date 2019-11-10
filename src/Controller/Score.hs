module Controller.Score (stepScore, eventScore) where

  import Graphics.Gloss.Interface.IO.Game
  import Model
  import Type.State
  import Type.IO.Input

  --Empty for score screen
  stepScore :: Float -> GameState -> IO GameState
  stepScore t = return

  eventScore :: Event -> GameState -> IO GameState
  eventScore e gs = return (checkModeSwitch gs)
  
  --Check if player pressed continue
  checkModeSwitch :: GameState -> GameState
  checkModeSwitch gs@GameState{inputState = ks}
    | keyDown ks Start = initialGameState (screen ks)
    | otherwise        = gs