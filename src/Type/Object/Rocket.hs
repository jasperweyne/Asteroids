module Type.Object.Rocket (Rocket(..), makeRocket) where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Class.HasGameObject
    import Class.Updateable
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

    instance Updateable Rocket where
        update x@Rocket{obj = o} f = x{obj = update o f}

    makeRocket :: Position -> Velocity -> Float -> Picture -> Rocket
    makeRocket p v r i = Rocket {
        obj = zeroGameObject {
            pos = p,
            vel = v,
            rot = r,
            radius = 10
        },
        picture = i
        }