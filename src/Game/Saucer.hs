module Game.Saucer where
  
  import Class.HasGameObject
  import Data.Maybe
  import Game.Object
  import IO.Queue
  import Type.Object.Saucer
  import Type.Object.Asteroid hiding (obj, picture)
  import Type.Physics.GameObject
  import Type.State

  updateSaucers :: Float -> GameState -> [Saucer]
  updateSaucers t gs@GameState{inGame = igs@InGameState{saucers = s, asteroids = as}} = map doEvade s
    where
      doEvade x = x `evade` (x `findDangerous` asteroids igs)

  evade :: HasGameObject x => x -> [Vector] -> x
  evade x [] = x 
  evade x xs = x `setGameObject` (getGameObject x) {
    vel = toVel (safeDir * Vec speed speed)
  }
    where
      speed = mag.velToVec.vel.getGameObject $ x
      safeDir = norm (foldr (+) zeroVec xs) * Vec (-1) (-1)
  
  findDangerous :: (HasGameObject x, HasGameObject y) => x -> [y] -> [Vector]
  findDangerous _ [] = [] 
  findDangerous x xs = mapMaybe isDangerous xs
    where
      isDangerous y | mag (getOffset x y) < x `closeTo` y = Just (getOffset x y)
                    | otherwise                           = Nothing
      getOffset a b = offset (pos.getGameObject $ a) (pos.getGameObject $ b)
      closeTo   a b = (radius.getGameObject $ a) * 2 + (radius.getGameObject $ b)

  postUpdateSaucers :: Float -> GameState -> GameState
  postUpdateSaucers t gs@GameState{inGame = igs@InGameState{saucers = s}} = updatedGs
    where
      updatedGs | null sx   = spawnSaucer newGs 
                | otherwise =             newGs
      newGs = gs{inGame = igs{
        saucers = sx
      }}
      sx = map (`wrapOutOfBounds` gs) s
      
  spawnSaucer :: GameState -> GameState
  spawnSaucer gs@GameState{inGame = igs@InGameState{saucers = s}} =
    let (spawn, r) = spawnOnBounds (randGen gs) 100 gs in
    gs{randGen = r, inGame = igs{
      saucers = Saucer {
        obj = spawn {
            radius = 25
        },
        picture = saucerPicture gs
      } : s
    }}
  
  
  
