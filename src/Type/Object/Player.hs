module Type.Object.Player where
    import Graphics.Gloss
    import Type.Physics.GameObject

    data Player = Player {
        obj :: GameObject,
        lives :: Int,
        render :: GameObject -> Picture
    }