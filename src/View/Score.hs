module View.Score (viewScore) where
  
  import Graphics.Gloss.Interface.IO.Game
  import Type.State
  import Type.IO.Scoreboard
  import Type.IO.Input

  viewScore :: GameState -> IO Picture
  viewScore = return . viewScorePure

  --Draw score screen
  viewScorePure :: GameState -> Picture
  viewScorePure gs@GameState{inputState = ks, highscores = sb, inGame = InGameState{score = s}} = 
    translate (-300) (h * 0.25) $ color white $ pictures 
      [
        text "You died..",
        translate 150 (-100) $ scale 0.5 0.5 $ text ("Score: " ++ show s),
        translate 150 (-170) $ scale 0.2 0.2 $ text "Press enter to restart..",
        translate 120 (-250) $ renderScores sb
      ]
    where
      w = fromIntegral (fst (screen ks))
      h = fromIntegral (snd (screen ks))
  
  --Draw scoreboard, loaded from gamestate
  renderScores :: Scoreboard -> Picture
  renderScores Scoreboard{scores = sc} = 
    pictures $ 
      map (\((name, score), i) -> 
        translate 0 (fromIntegral i * (-50)) (scale 0.2 0.2 (text (show score ++ ", " ++ name)))) 
        (zip sc [0..(min (length sc) 5)])
  