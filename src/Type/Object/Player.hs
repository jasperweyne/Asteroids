module Type.Object.Player where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Type.Physics.GameObject

    data Player = Player {
        obj :: GameObject,
        lives :: Int,
        renderFn :: GameObject -> Picture
    }

    instance Renderable Player where
        render x = (renderFn x) (obj x)
