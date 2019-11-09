module View.Score (viewScore) where
  
  import Graphics.Gloss.Interface.IO.Game
  import Type.State

  viewScore :: GameState -> IO Picture
  viewScore = return . viewScorePure

  viewScorePure :: GameState -> Picture
  viewScorePure _ = translate (-250) 0 $ color white $ pictures 
    [
      text "Asteroids",
      translate 100 (-100) $ scale 0.5 0.5 $ text "-Highscores-",
      translate 50 (-150) $ scale 0.1 0.1 $ text "[Score here]",
      translate 50 (-200) $ scale 0.1 0.1 $ text "[Score here]"
    ]
  