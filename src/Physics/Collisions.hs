module Physics.Collisions where
    
  explode :: Asteroid -> [Asteroid]

  collides :: GameObject -> GameObject -> Bool

  playerHitAsteroids :: Player -> [Asteroid] -> Player --reduce lives
  playerHitSaucers :: Player -> [Saucer] -> Player --reduce lives
  rocketHitAsteroids :: Rocket -> [Asteroid] -> Maybe Rocket
  rocketHitSaucers :: Rocket -> [Saucer] -> Maybe Rocket
  asteroidHitRockets :: Asteroid -> [Rockets] -> [Asteroid]
  saucerHitRockets :: Saucer -> [Rockets] -> Maybe Saucer