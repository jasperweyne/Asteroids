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
  
  collides :: GameObject -> GameObject -> Bool
  collides o1 o2 = d <= 0
    where
      d = distance (pos o1) (pos o2) - radius o1 - radius o2

  collidesWith :: (HasGameObject x, HasGameObject y) => x -> [y] -> Bool
  collidesWith x = any (\y -> getGameObject x `collides` getGameObject y)

  removeOnCollision :: (HasGameObject x, HasGameObject y) => x -> [y] -> Maybe x
  removeOnCollision x ys | x `collidesWith` ys = Nothing
                         | otherwise           = Just x 

  asteroidHitPlayer :: Asteroid -> Player -> [Asteroid]
  asteroidHitPlayer as p
    | as `collidesWith` [p] = Asteroid.branchAsteroid as
    | otherwise = [as]

  --playerHitSaucers :: Player -> [Saucer] -> Player --reduce lives
  rocketHitAsteroids :: Rocket -> [Asteroid] -> Maybe Rocket
  rocketHitAsteroids r as
    | r `collidesWith` as = Nothing
    | otherwise = Just r

  rocketHitPlayer :: Rocket -> Player -> Maybe Rocket
  rocketHitPlayer r p
    | r `collidesWith` [p] = Nothing
    | otherwise = Just r

  --rocketHitSaucers :: Rocket -> [Saucer] -> Maybe Rocket
  asteroidHitRockets :: Asteroid -> [Rocket] -> [Asteroid]
  asteroidHitRockets as rs
    | as `collidesWith` rs = Asteroid.branchAsteroid as
    | otherwise = [as]
  --saucerHitRockets :: Saucer -> [Rockets] -> Maybe Saucer