module View.Menu (viewMenu) where
  
  import Graphics.Gloss.Interface.IO.Game
  import Type.State

  viewMenu :: GameState -> IO Picture
  viewMenu = return . viewMenuPure

  --Show start screen
  viewMenuPure :: GameState -> Picture
  viewMenuPure gstate = translate (-250) 0 $ color white $ pictures 
    [
      text "Asteroids",
      translate (-50) (-100) $ scale 0.5 0.5 $ text "Press enter to start..",
      translate 20 (-200) $ scale 0.3 0.3 $ text "Press escape for settings",
      translate 0 (-300) $ scale 0.2 0.2 $ text "Made by Silvan Eelman and Jasper Weyne"
    ]