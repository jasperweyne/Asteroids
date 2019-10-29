module Type.IO.Scoreboard where
    
  data Scoreboard = Scoreboard {
    scores :: [(String, Int)]
  }