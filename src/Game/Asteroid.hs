module Game.Asteroid (updateAsteroids, postUpdateAsteroids, attemptAsteroidSpawns) where
  
  import Class.HasGameObject
  import Data.Maybe
  import System.Random (RandomGen, randomR)
  import Game.Object
  import Graphics.Gloss
  import Type.Object.Asteroid
  import Type.State
  import Type.IO.Input
  import Type.Physics.GameObject
  import Physics.Collisions
  
  --Check asteroid collisions
  updateAsteroids :: Float -> GameState -> [Asteroid]
  updateAsteroids t gs@GameState{inGame = igs@InGameState{asteroids = as, player = p, sRockets = srs, pRockets = prs}} = as4
    where
      as2 = concat ((`splitOnCollision` [p]) <$> as)
      as3 = concat ((`splitOnCollision` prs) <$> as2)
      as4 = concat ((`splitOnCollision` srs) <$> as3)
    
  --Split asteroid when it hits an object
  splitOnCollision :: HasGameObject x => Asteroid -> [x] -> [Asteroid]
  splitOnCollision as xs
    | as `collidesWith` xs = branchAsteroid as
    | otherwise = [as]

  --Remove outOfBound asteroids and spawn new ones
  postUpdateAsteroids :: Float -> GameState -> GameState
  postUpdateAsteroids t gs1@GameState{inGame = igs@InGameState{asteroids = as1}} = gs3
    where
      as2 = mapMaybe (`removeOutOfBounds` gs1) as1
      gs2 = gs1{inGame = igs{asteroids = as2}}
      gs3 = attemptAsteroidSpawns t gs2

  --Attempt asteroid spawn using time-adjusted probability
  attemptAsteroidSpawns :: Float -> GameState -> GameState
  attemptAsteroidSpawns t gs@GameState{inputState = is, inGame = igs@InGameState{asteroids = as}, asteroidPicture = ap}
    | r < p && length as < 10 = let (newAs, g2) = spawnAtBorder g1 gs ap in 
      gs{inGame = igs{asteroids = as ++ [newAs]}, randGen = g2}
    | otherwise = gs{randGen = g1}
    where
      p = t / 6
      (r, g1) = randomR (0, 1) (randGen gs)

  --Spawn asteroid just beyond the screen border
  spawnAtBorder :: RandomGen g => g -> GameState -> Picture -> (Asteroid, g)
  spawnAtBorder g1 gs x = (makeAsteroid 3 p v r x, g3)
    where
      (go, g2) = spawnOnBounds g1 20 gs
      p = pos go
      v = vel go
      (r, g3) = randomR (0, 2 * pi) g2
