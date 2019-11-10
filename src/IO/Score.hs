module IO.Score (loadScoreboard, saveScoreboard) where
  
  import Graphics.Gloss.Interface.IO.Game
  import Data.List.Split
  import Data.Maybe
  import Text.Read
  import Type.IO.Scoreboard
  import Type.State
  import System.Directory

  loadScoreboard :: FilePath -> GameState -> IO GameState
  loadScoreboard fp gs = do 
                        e <- doesFileExist fp 
                        if e 
                          then load fp
                          else return gs

    where
      load :: FilePath -> IO GameState
      load fp = do 
                  txt <- readFile fp
                  let ln = lines txt in
                    return gs{highscores = Scoreboard { scores = scoreSort (mapMaybe splitNameScore ln)}}   

      splitNameScore :: String -> Maybe (String, Int)
      splitNameScore t = case splitOn "," t of
        [s1, s2] -> do 
                      i <- readMaybe s2
                      return (s1, i)
        _        -> Nothing


  saveScoreboard :: GameState -> IO ()
  saveScoreboard gs@GameState{highscores = sb} = writeFile "highscores.txt" (show sb)