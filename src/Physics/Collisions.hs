module Physics.Collisions (collides, playerHitAsteroids) where
  import Type.Physics.GameObject
  import Type.Object.Player as Player
  import Type.Object.Asteroid as Asteroid
  --explode :: Asteroid -> [Asteroid]

  collides :: GameObject -> GameObject -> Bool
  collides o1 o2 = d <= 0
      where
        d = distance (pos o1) (pos o2) - radius o1 - radius o2

  playerHitAsteroids :: Player -> [Asteroid] -> Player --reduce lives
  playerHitAsteroids p [] = p
  playerHitAsteroids p@Player{Player.obj = o, lives = l} ast
    | any (collides o) (Asteroid.obj <$> ast) = p{lives = newLives}
    | otherwise = p
    where
      newLives = l - 1

  --playerHitSaucers :: Player -> [Saucer] -> Player --reduce lives
  --rocketHitAsteroids :: Rocket -> [Asteroid] -> Maybe Rocket
  --rocketHitSaucers :: Rocket -> [Saucer] -> Maybe Rocket
  --asteroidHitRockets :: Asteroid -> [Rockets] -> [Asteroid]
  --saucerHitRockets :: Saucer -> [Rockets] -> Maybe Saucer