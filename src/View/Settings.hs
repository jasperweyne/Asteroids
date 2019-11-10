module View.Settings (viewSettings) where
  
  import Graphics.Gloss.Interface.IO.Game
  import Type.State
  import Type.IO.Input

  viewSettings :: GameState -> IO Picture
  viewSettings = return . viewSettingsPure

  --Draw settings screen
  viewSettingsPure :: GameState -> Picture
  viewSettingsPure gstate = translate (-250) 0 $ color white $ pictures 
    [
      text "Settings",
      translate 0 (-100) $ scale 0.5 0.5 $ text ("Input: " ++ (show . inputmode . inputState) gstate),
      translate 0 (-200) $ scale 0.2 0.2 $ text "Press enter to switch input mode",
      translate 0 (-250) $ scale 0.2 0.2 $ text "Press escape to return to menu"
    ]