module Type.Object.Asteroid where
    import Type.Physics.GameObject

    data Asteroid = Asteroid {
        obj :: GameObject,
        level :: Int
    }