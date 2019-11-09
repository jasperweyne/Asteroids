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
  
  updateAsteroids :: Float -> GameState -> [Asteroid]
  updateAsteroids t gs@GameState{inGame = igs@InGameState{asteroids = as, player = p}} = 
      concat ((`asteroidHitPlayer` p) <$> as)
    
  postUpdateAsteroids :: Float -> GameState -> GameState
  postUpdateAsteroids t gs1@GameState{inGame = igs@InGameState{asteroids = as1}} = attemptAsteroidSpawns t gs2
    where
      as2 = mapMaybe (`removeOutOfBounds` gs1) as1
      gs2 = gs1{inGame = igs{asteroids = as2}}

  attemptAsteroidSpawns :: Float -> GameState -> GameState
  attemptAsteroidSpawns t gs@GameState{inputState = is, inGame = igs@InGameState{asteroids = as}, asteroidPicture = ap}
      | r < p = let (newAs, g2) = spawnAtBorder g1 gs ap in 
                  gs{inGame = igs{asteroids = as ++ [newAs]}, randGen = g2}
      | otherwise = gs{randGen = g1}
    where
      p = t / 3
      (r, g1) = randomR (0, 1) (randGen gs)

  spawnAtBorder :: RandomGen g => g -> GameState -> Picture -> (Asteroid, g)
  spawnAtBorder g1 gs x = (makeAsteroid 3 p v r x, g3)
    where
      (go, g2) = spawnOnBounds g1 20 gs
      p = pos go
      v = vel go
      (r, g3) = randomR (0, 2 * pi) g2
