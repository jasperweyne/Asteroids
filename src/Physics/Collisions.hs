module Physics.Collisions (collides, playerHitAsteroids, asteroidHitPlayer, rocketHitAsteroids, asteroidHitRockets, saucerHitAsteroids) where
  
  import Class.HasGameObject
  import Type.Physics.GameObject
  import Type.Object.Player as Player
  import Type.Object.Asteroid as Asteroid
  import Type.Object.Saucer as Saucer
  import Type.Object.Rocket as Rocket
  import Type.Object.Explosion as Explosion
  
  collides :: GameObject -> GameObject -> Bool
  collides o1 o2 = d <= 0
    where
      d = distance (pos o1) (pos o2) - radius o1 - radius o2

  collidesWith :: (HasGameObject x, HasGameObject y) => x -> [y] -> Bool
  collidesWith x = any (\y -> getGameObject x `collides` getGameObject y)

  playerHitAsteroids :: Player -> [Asteroid] -> Player --reduce lives
  playerHitAsteroids p@Player{Player.obj = o, lives = l} ast
    | p `collidesWith` ast = p{lives = newLives, Player.obj = newPlyObj}
    | otherwise = p
    where
      newLives = l - 1
      newPlyObj = foldl (\x y -> if collides x y then 
          x{pos = pos x + toPos (offset (pos y) (pos x))} 
        else x) o{pos = Pos 0 0, vel = Vel 0 0} asObjs 
      asObjs = Asteroid.obj <$> ast

  asteroidHitPlayer :: Asteroid -> Player -> [Asteroid]
  asteroidHitPlayer as p
    | as `collidesWith` [p] = Asteroid.branchAsteroid as
    | otherwise = [as]

    
  saucerHitAsteroids :: Saucer -> [Asteroid] -> Either Saucer Saucer
  saucerHitAsteroids s ast
    | s `collidesWith` ast = Left s
    | otherwise            = Right s

  --playerHitSaucers :: Player -> [Saucer] -> Player --reduce lives
  rocketHitAsteroids :: Rocket -> [Asteroid] -> Maybe Rocket
  rocketHitAsteroids r as
    | r `collidesWith` as = Nothing
    | otherwise = Just r

  --rocketHitSaucers :: Rocket -> [Saucer] -> Maybe Rocket
  asteroidHitRockets :: Asteroid -> [Rocket] -> [Asteroid]
  asteroidHitRockets as rs
    | as `collidesWith` rs = Asteroid.branchAsteroid as
    | otherwise = [as]
  --saucerHitRockets :: Saucer -> [Rockets] -> Maybe Saucer