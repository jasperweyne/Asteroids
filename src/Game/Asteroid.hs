module Game.Asteroid (updateAsteroids, postUpdateAsteroids) where
  import Data.Maybe
  import Game.Object
  import Graphics.Gloss
  import Type.Object.Asteroid
  import Type.State
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
  postUpdateAsteroids t gs@GameState{inGame = igs@InGameState{asteroids = as, player = p}} = gs{inGame = igs{
    asteroids = mapMaybe (`removeOutOfBounds` gs) as
  }}