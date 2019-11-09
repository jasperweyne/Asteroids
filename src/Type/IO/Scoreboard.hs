module Type.IO.Scoreboard where
    
  newtype Scoreboard = Scoreboard {
    scores :: [(String, Int)]
  }