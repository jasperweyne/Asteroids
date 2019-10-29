module Type.Object.Asteroid where

    import Graphics.Gloss
    --import Class.Rendering.Renderable
    import Type.Physics.GameObject

    data Asteroid = Asteroid {
        obj :: GameObject,
        level :: Int,
        render :: GameObject -> Picture
    }

    --instance Renderable Asteroid where
    --    render :: GameObject -> Picture