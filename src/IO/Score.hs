module IO.Score where
  
  loadScoreboard :: GameState -> IO GameState
  saveScoreboard :: (String, Int) -> GameState -> IO GameState