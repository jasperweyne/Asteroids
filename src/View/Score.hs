module View.Score (viewScore) where
  
  import Graphics.Gloss.Interface.IO.Game
  import Type.State
  import Type.IO.Scoreboard

  viewScore :: GameState -> IO Picture
  viewScore = return . viewScorePure

  viewScorePure :: GameState -> Picture
  viewScorePure gs@GameState{highscores = sb, inGame = InGameState{score = s}} = translate (-250) 0 $ color white $ pictures 
    [
      text "You died..",
      translate 100 (-100) $ scale 0.5 0.5 $ text ("Score: " ++ show s),
      translate 100 (-200) $ renderScores sb]
  
  renderScores :: Scoreboard -> Picture
  renderScores Scoreboard{scores = sc} = pictures $ map (\((name, score), i) -> translate 0 (fromIntegral i * (-50)) (scale 0.2 0.2 (text (show score ++ ", " ++ name)))) (zip sc [0..(length sc)])
  