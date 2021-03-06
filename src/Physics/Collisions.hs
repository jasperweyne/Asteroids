module Physics.Collisions where
  
  import Class.HasGameObject
  import Data.Either
  import Data.Bifunctor
  import Type.Physics.GameObject
  import Type.Object.Player as Player
  import Type.Object.Asteroid as Asteroid
  import Type.Object.Saucer as Saucer
  import Type.Object.Rocket as Rocket
  import Type.Object.Explosion as Explosion
  import Type.Rendering.Animation
  
  --Check if two objects collide
  collides :: GameObject -> GameObject -> Bool
  collides o1 o2 = d <= 0
    where
      d = distance (pos o1) (pos o2) - radius o1 - radius o2

  --Check if object A collides with any B
  collidesWith :: (HasGameObject x, HasGameObject y) => x -> [y] -> Bool
  collidesWith x = any (\y -> getGameObject x `collides` getGameObject y)

  --Remove object A if colliding with any B
  removeOnCollision :: (HasGameObject x, HasGameObject y) => x -> [y] -> Maybe x
  removeOnCollision x ys | x `collidesWith` ys = Nothing
                         | otherwise           = Just x 
