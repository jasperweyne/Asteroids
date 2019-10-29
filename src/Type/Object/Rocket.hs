module Type.Object.Rocket where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Type.Physics.GameObject
    
    data Rocket = Rocket {
        obj :: GameObject,
        renderFn :: GameObject -> Picture
    }

    instance Renderable Rocket where
        render x = (renderFn x) (obj x)