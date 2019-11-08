module Type.Object.Asteroid (Asteroid(..), makeAsteroid, branchAsteroid) where

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
        getGameObject = obj
        setGameObject x o = x { obj = o }

    instance Updateable Asteroid where
        update x@Asteroid{obj = o@GameObject{rot = r}} f = x{obj = update o{rot = newRot} f}
            where 
                newRot = r + f * 0.3 * (4 - fromIntegral (level x))

    makeAsteroid :: Int -> Position -> Velocity -> Float -> Picture -> Asteroid
    makeAsteroid l p v r i = Asteroid {
        obj = zeroGameObject {
            pos = p,
            vel = v,
            rot = r,
            radius = 25
        },
        level = l,
        picture = i
        }
    
    branchAsteroid :: Asteroid -> [Asteroid]
    branchAsteroid as@Asteroid{obj = o, level = l, picture = pic} = 
        [makeAsteroid (l - 1) pos1 vel1 newRad pic,
         makeAsteroid (l - 1) pos2 vel2 newRad pic,
         makeAsteroid (l - 1) pos3 vel3 newRad pic]
            where
                pos1 = toPos $ posToVec (pos o) + dir 0 * Vec newRad newRad
                pos2 = toPos $ posToVec (pos o) + dir (pi / 1.5) * Vec newRad newRad
                pos3 = toPos $ posToVec (pos o) + dir ((pi / 3) * 4) * Vec newRad newRad
                vel1 = toVel $ velToVec (vel o) + dir 0 * Vec 10 10
                vel2 = toVel $ velToVec (vel o) + dir (pi / 1.5) * Vec 10 10
                vel3 = toVel $ velToVec (vel o) + dir ((pi / 3) * 4) * Vec 10 10
                dir d = Vec (sin (d + rot o)) (cos (d + rot o))
                newRad = radius o / 2
