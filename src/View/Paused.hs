module View.Paused (viewPaused) where
  
  import Graphics.Gloss.Interface.IO.Game
  import Type.State

  viewPaused :: GameState -> IO Picture
  viewPaused = return . viewPausedPure

  --Show paused screen
  viewPausedPure :: GameState -> Picture
  viewPausedPure _ = translate (-250) 0 $ color white $ pictures 
    [
      text "Asteroids",
      translate 100 (-100) $ scale 0.5 0.5 $ text "-Paused-",
      translate 100 (-200) $ scale 0.2 0.2 $ text "Press escape to resume"
    ]
