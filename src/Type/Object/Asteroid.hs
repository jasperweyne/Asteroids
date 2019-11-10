module Type.Object.Asteroid (Asteroid(..), makeAsteroid, branchAsteroid) where

  import Graphics.Gloss
  import Class.Renderable
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
    render x = let picScale = fromIntegral (level x) in
      renderFactory (scale picScale picScale (picture x)) (obj x)
      
  instance HasGameObject Asteroid where
    getGameObject = obj
    setGameObject x o = x { obj = o }

  instance Updateable Asteroid where
    update x@Asteroid{obj = o@GameObject{rot = r}} f = x{obj = update o{rot = newRot} f}
      where 
        newRot = r + f * 0.3 * (4 - fromIntegral (level x))

  --Create asteroid objecjt
  makeAsteroid :: Int -> Position -> Velocity -> Float -> Picture -> Asteroid
  makeAsteroid l p v r i = Asteroid {
    obj = zeroGameObject {
      pos = p,
      vel = v,
      rot = r,
      radius = 25 * fromIntegral l
    },
    level = l,
    picture = i
  }
  
  --Split asteroid into 3 asteroids
  branchAsteroid :: Asteroid -> [Asteroid]
  branchAsteroid as@Asteroid{obj = o, level = l, picture = pic}
    | l > 1 = [makeAsteroid (l - 1) (mkPos r1) (mkVel r1) r1 pic,
               makeAsteroid (l - 1) (mkPos r2) (mkVel r2) r2 pic,
               makeAsteroid (l - 1) (mkPos r3) (mkVel r3) r3 pic]
    | otherwise = []
      where
        mkPos r = toPos $ posToVec (pos o) + dir r * Vec newRad newRad
        mkVel r = toVel $ velToVec (vel o) + dir r * Vec 10 10
        dir r = Vec (sin (r + rot o)) (cos (r + rot o))
        r1 = rot o
        r2 = rot o + (pi / 1.5)
        r3 = rot o + ((pi / 3) * 4)
        newRad = radius o / 2
