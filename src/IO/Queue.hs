module IO.Queue (queueIO) where
    import Type.State

    --Queues IO-action into gamestate
    queueIO :: GameState -> (GameState -> IO GameState) -> GameState
    queueIO gs fn = gs { processIO = \x -> processIO gs x >>= fn }
