module IO.Random (generateStdRandom) where

  import System.Random (getStdGen)
  import Type.State

  --Puts an Std random into the gamestate
  generateStdRandom :: GameState -> IO GameState
  generateStdRandom gs = do 
    rand <- getStdGen
    return gs{randGen = rand}