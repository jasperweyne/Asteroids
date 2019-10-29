module Type.Object.Saucer where

    import Graphics.Gloss
    --import Class.Rendering.Renderable
    import Type.Physics.GameObject

    data Saucer = Saucer {
        obj :: GameObject,
        render :: GameObject -> Picture
    }

    --instance Renderable Asteroid where
    --    render :: GameObject -> Picture
