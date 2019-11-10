module IO.Score (loadScoreboard, saveScoreboard) where
  
  import Graphics.Gloss.Interface.IO.Game
  import Data.List.Split
  import Data.Maybe
  import Text.Read
  import Type.IO.Scoreboard
  import Type.State
  import System.Directory
  import System.IO.Strict as Strict
  import Data.Time

  --Load scoreboard into gamestate
  loadScoreboard :: FilePath -> GameState -> IO GameState
  loadScoreboard fp gs = do 
                        e <- doesFileExist fp 
                        if e 
                          then load fp
                          else return gs
    where
      --Load scoreboard from file using strict IO, required to release file immediatly after reading
      load :: FilePath -> IO GameState
      load fp = do 
                  txt <- run (Strict.readFile fp)
                  let ln = lines txt in
                    return gs{highscores = Scoreboard { scores = scoreSort (mapMaybe splitNameScore ln)}}   

      --Split name and score from string
      splitNameScore :: String -> Maybe (String, Int)
      splitNameScore t = case splitOn "," t of
        [s1, s2] -> do 
                      i <- readMaybe s2
                      return (s1, i)
        _        -> Nothing

  --Save entire scoreboard to disk, overwriting previous file
  saveScoreboard :: FilePath -> GameState -> IO GameState
  saveScoreboard fp gs@GameState{highscores = sb} = do 
                                                  zt <- getZonedTime
                                                  let newSb = Scoreboard {
                                                    scores = scoreSort (("[" ++ takeWhile (/= '.') (show zt) ++ "]", (score.inGame)gs) : scores sb)
                                                    } in
                                                    run (Strict.writeFile fp (show newSb))
                                                  return gs