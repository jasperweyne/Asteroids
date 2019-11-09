module IO.Random (generateStdRandom) where

  import System.Random (getStdGen)
  import Type.State

  generateStdRandom :: GameState -> IO GameState
  generateStdRandom gs = do 
    rand <- getStdGen
    return gs{randGen = rand}