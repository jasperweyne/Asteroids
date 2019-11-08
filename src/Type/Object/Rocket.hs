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
        getGameObject = obj
        setGameObject x o = x { obj = o }