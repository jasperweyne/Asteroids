module Type.Object.Rocket where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Type.Physics.GameObject
    
    data Rocket = Rocket {
        obj :: GameObject,
        render :: GameObject -> Picture
    }

    instance Renderable Asteroid where
        render :: GameObject -> Picture
