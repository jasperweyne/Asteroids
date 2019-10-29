module IO.Input where
    
  renderFactory :: Picture -> (GameObject -> Picture) --Factory method for render function in player, asteroid, etc.
  queueIO :: GameState -> (GameState -> IO GameState) -> GameState --Queues IO-action over gamestate

  loadPlayerPicture :: GameState -> IO GameState
  loadAsteroidPicture :: GameState -> IO GameState
  loadScoreboard :: GameState -> IO GameState
  saveScoreboard :: (String, Int) -> GameState -> IO GameState