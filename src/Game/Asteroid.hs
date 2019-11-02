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
        | null as = makeAsteroid 3 (Pos 100 100) (Vel (-30) 20) 0.5 ap : 
                    makeAsteroid 3 (Pos (-300) (-100)) (Vel 20 10) (-1.7) ap : 
                    makeAsteroid 3 (Pos 400 400) (Vel (-20) (-15)) 1.2 ap :
                    makeAsteroid 3 (Pos (-500) 300) (Vel 50 (-30)) 3 ap : as
        | otherwise = as

  updateAsteroid :: Asteroid -> GameState -> Float -> Asteroid
  updateAsteroid a gs t = a

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