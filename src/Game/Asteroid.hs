module Game.Asteroid (updateAsteroids, postUpdateAsteroids) where
  import Data.Maybe
  import System.Random (RandomGen, randomR)
  import Game.Object
  import Graphics.Gloss
  import Type.Object.Asteroid
  import Type.State
  import Type.IO.Input
  import Type.Physics.GameObject
  import Physics.Collisions
  
  --explode :: Asteroid -> [Asteroid]

  updateAsteroids :: Float -> GameState -> [Asteroid]
  updateAsteroids t gs@GameState{inGame = igs@InGameState{asteroids = as, player = p}, asteroidPicture = ap} = 
      concat ((`asteroidHitPlayer` p) <$> newAs)
    where 
      newAs
        | null as = makeAsteroid 3 (Pos 100 100) (Vel (-30) 20) 0.5 ap : 
                    makeAsteroid 3 (Pos (-300) (-100)) (Vel 20 10) (-1.7) ap : 
                    makeAsteroid 3 (Pos 400 400) (Vel (-20) (-15)) 1.2 ap :
                    makeAsteroid 3 (Pos (-500) 300) (Vel 50 (-30)) 3 ap : as
        | otherwise = as
    
  postUpdateAsteroids :: Float -> GameState -> GameState
  postUpdateAsteroids t gs@GameState{inGame = igs@InGameState{asteroids = as, player = p}} = attemptAsteroidSpawns t gs{inGame = igs{
    asteroids = mapMaybe (`removeOutOfBounds` gs) as
  }}

  attemptAsteroidSpawns :: Float -> GameState -> GameState
  attemptAsteroidSpawns t gs@GameState{inputState = is, inGame = igs@InGameState{asteroids = as}, asteroidPicture = ap}
      | r < p = let (newAs, g2) = spawnAtBorder g1 (screen is) ap in 
                  gs{inGame = igs{asteroids = as ++ [newAs]}, randGen = g2}
      | otherwise = gs{randGen = g1}
    where
      p = t / 3
      (r, g1) = randomR (0, 1) (randGen gs)

  spawnAtBorder :: RandomGen g => g -> (Int, Int) -> Picture -> (Asteroid, g)
  spawnAtBorder g1 (iw, ih) p = (makeAsteroid 3 (Pos px py) (toVel (Vec 20 20 * norm v)) r p, g5)
    where
      w = fromIntegral iw
      h = fromIntegral ih
      (pt, g2) = randomR (0, 2 * w + 2 * h) g1
      (px, py)
        | pt < w = (pt, h * (-0.5))
        | pt < w * 2 = (pt - w, h * 0.5)
        | pt < w * 2 + h = (w * (-0.5), pt - w * 2)
        | otherwise = (w * 0.5, pt - (w * 2 + h))
      (tx, g3) = randomR (w * (-0.25), w * 0.25) g2
      (ty, g4) = randomR (h * (-0.25), h * 0.25) g3 
      v = Vec (tx - px) (ty - py)
      (r, g5) = randomR (0, 2 * pi) g4
