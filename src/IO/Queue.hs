module IO.Queue where
  queueIO :: GameState -> (GameState -> IO GameState) -> GameState --Queues IO-action over gamestate
