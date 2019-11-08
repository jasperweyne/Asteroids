module Type.Object.Asteroid where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Class.HasGameObject
    import Class.Updateable
    import Rendering.GameObject
    import Type.Physics.GameObject

    data Asteroid = Asteroid {
        obj :: GameObject,
        level :: Int,
        picture :: Picture
    }

    instance Renderable Asteroid where
        render x = renderFactory (picture x) (obj x)
        
    instance HasGameObject Asteroid where
        get_gameobject = obj
        set_gameobject x o = x { obj = o }

    instance Updateable Asteroid where
        update x@Asteroid{obj = o@GameObject{rot = r}} f = x{obj = update o{rot = newRot} f}
            where 
                newRot = r + f * 0.3 * (4 - fromIntegral (level x))
