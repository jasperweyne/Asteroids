module Type.Object.Asteroid where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Type.Physics.GameObject

    data Asteroid = Asteroid {
        obj :: GameObject,
        level :: Int,
        renderFn :: GameObject -> Picture
    }

    instance Renderable Asteroid where
        render x = (renderFn x) (obj x)