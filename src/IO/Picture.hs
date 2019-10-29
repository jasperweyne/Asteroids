module IO.Picture where
  renderFactory :: Picture -> (GameObject -> Picture) --Factory method for render function in player, asteroid, etc.
  loadPlayerPicture :: GameState -> IO GameState
  loadAsteroidPicture :: GameState -> IO GameState