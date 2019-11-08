module Physics.Collisions (collides, playerHitAsteroids, asteroidHitPlayer) where
  import Type.Physics.GameObject
  import Type.Object.Player as Player
  import Type.Object.Asteroid as Asteroid
  
  collides :: GameObject -> GameObject -> Bool
  collides o1 o2 = d <= 0
      where
        d = distance (pos o1) (pos o2) - radius o1 - radius o2

  playerHitAsteroids :: Player -> [Asteroid] -> Player --reduce lives
  playerHitAsteroids p [] = p
  playerHitAsteroids p@Player{Player.obj = o, lives = l} ast
    | any (collides o) asObjs = p{lives = newLives, Player.obj = newPlyObj}
    | otherwise = p
    where
      newLives = l - 1
      newPlyObj = foldl (\x y -> if collides x y then 
          x{pos = pos x + toPos (offset (pos y) (pos x))} 
        else x) o{pos = Pos 0 0, vel = Vel 0 0} asObjs 
      asObjs = Asteroid.obj <$> ast

  asteroidHitPlayer :: Asteroid -> Player -> [Asteroid]
  asteroidHitPlayer as p
    | collides (Asteroid.obj as) (Player.obj p) = Asteroid.branchAsteroid as
    | otherwise = [as]

  --playerHitSaucers :: Player -> [Saucer] -> Player --reduce lives
  --rocketHitAsteroids :: Rocket -> [Asteroid] -> Maybe Rocket
  --rocketHitSaucers :: Rocket -> [Saucer] -> Maybe Rocket
  --asteroidHitRockets :: Asteroid -> [Rockets] -> [Asteroid]
  --saucerHitRockets :: Saucer -> [Rockets] -> Maybe Saucer