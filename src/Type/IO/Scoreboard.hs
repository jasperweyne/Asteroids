module Type.IO.Scoreboard (Scoreboard(..), scoreSort) where

  import Data.List
  import Data.Function
  
  scoreSort :: [(String, Int)] -> [(String, Int)]
  scoreSort = sortBy (flip compare `on` snd) 

  newtype Scoreboard = Scoreboard {
    scores :: [(String, Int)]
  }

  instance Show Scoreboard where
    show sb@Scoreboard{scores = sc} = unlines $ map (\(x, y) -> x ++ ", " ++ show y) sc