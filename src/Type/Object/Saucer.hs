module Type.Object.Saucer where
    
    import Type.Physics.Body

    data Saucer = Saucer {
        loc :: Position,
        v   :: Velocity
    }