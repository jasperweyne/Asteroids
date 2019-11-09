module View.Menu (viewMenu) where
  
  import Graphics.Gloss.Interface.IO.Game
  import Type.State

  viewMenu :: GameState -> IO Picture
  viewMenu = return . viewMenuPure

  viewMenuPure :: GameState -> Picture
  viewMenuPure gstate = translate (-250) 0 $ color white $ pictures 
    [
      text "Asteroids",
      translate (-50) (-100) $ scale 0.5 0.5 $ text "Press enter to start..",
      translate 0 (-200) $ scale 0.2 0.2 $ text "Made by Silvan Eelman and Jasper Weyne"
    ]