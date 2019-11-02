module Game.Asteroid (updateAsteroids) where
  import Graphics.Gloss
  import Type.Object.Asteroid
  import Type.State
  import Type.Physics.GameObject
  
  --explode :: Asteroid -> [Asteroid]

  updateAsteroids :: Float -> GameState -> GameState
  updateAsteroids t gs@GameState{inGame = igs@InGameState{asteroids = as}, asteroidPicture = ap} = gs{inGame = igs{asteroids = (\x -> updateAsteroid x gs t) <$> newAs}}
    where 
      newAs
        | null as = makeAsteroid 3 (Pos 100 100) (Vel 50 50) ap : as
        | otherwise = as

  updateAsteroid :: Asteroid -> GameState -> Float -> Asteroid
  updateAsteroid a gs t = a

  makeAsteroid :: Int -> Position -> Velocity -> Picture -> Asteroid
  makeAsteroid l p v i = Asteroid {
      obj = zeroGameObject {
          pos = p,
          vel = v
      },
      level = l,
      picture = i
    }