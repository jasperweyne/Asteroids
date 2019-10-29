module Type.Object.Player where
    import Grpahics.Gloss
    import Type.Physics.GameObject

    data Player = Player {
        obj :: GameObject,
        lives :: Int,
        render :: GameObject -> Picture
    }