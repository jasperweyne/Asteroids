module Type.Object.Saucer where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Type.Physics.GameObject

    data Saucer = Saucer {
        obj :: GameObject,
        renderFn :: GameObject -> Picture
    }

    instance Renderable Saucer where
        render x = (renderFn x) (obj x)
