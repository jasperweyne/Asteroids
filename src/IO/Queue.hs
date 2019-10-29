module IO.Queue (queueIO) where
    import Type.State

    --Queues IO-action into gamestate
    queueIO :: GameState -> (GameState -> IO GameState) -> GameState
    queueIO gs fn = gs { processIO = link (processIO gs) fn }
        where link f1 f2 = f1 >>= f2
