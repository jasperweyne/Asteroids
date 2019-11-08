module Type.Object.Rocket where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Class.HasGameObject
    import Rendering.GameObject
    import Type.Physics.GameObject
    
    data Rocket = Rocket {
        obj :: GameObject,
        picture :: Picture
    }

    instance Renderable Rocket where
        render x = renderFactory (picture x) (obj x)
        
    instance HasGameObject Rocket where
        get_gameobject = obj
        set_gameobject x o = x { obj = o }