module IO.Queue (queueIO) where
  
  import Control.Monad
  import Type.State

  --Queues IO-action into gamestate
  queueIO :: GameState -> (GameState -> IO GameState) -> GameState
  queueIO gs fn = gs { processIO = processIO gs >=> fn }
