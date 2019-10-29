module Type.Object.Asteroid (explode) where
    import Type.Physics.GameObject

    data Asteroid = Asteroid {
        obj :: GameObject,
        level :: Int
    }