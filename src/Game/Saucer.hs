module Game.Saucer where
  import Class.HasGameObject
  import Data.Maybe
  import Game.Object
  import IO.Queue
  import Type.Object.Saucer
  import Type.Object.Asteroid hiding (obj, picture)
  import Type.Physics.GameObject
  import Type.State

  preUpdateSaucers :: Float -> GameState -> GameState
  preUpdateSaucers t gs@GameState{inGame = igs@InGameState{saucers = s, asteroids = as}} = gs { inGame = igs {
    saucers = map doEvade s
  }}
    where
      doEvade x = x `evade` [x `findDangerous` asteroids igs]

  evade :: HasGameObject x => x -> [[Vector]] -> x
  evade x [] = x 
  evade x xs = x `setGameObject` (getGameObject x) {
    vel = toVel (safeDir * Vec speed speed)
  }
    where
      speed = -1 * (mag.velToVec.vel.getGameObject $ x)
      safeDir = norm (foldr (+) zeroVec (concat xs))
  
  findDangerous :: (HasGameObject x, HasGameObject y) => x -> [y] -> [Vector]
  findDangerous _ [] = [] 
  findDangerous x xs = mapMaybe isDangerous xs
    where
      isDangerous y | mag (getOffset x y) < 100 = Just (getOffset x y)
                    | otherwise                 = Nothing
      getOffset a b = offset (pos.getGameObject $ a) (pos.getGameObject $ b)

  postUpdateSaucers :: Float -> GameState -> GameState
  postUpdateSaucers t gs@GameState{inGame = igs@InGameState{saucers = s}} = updatedGs
    where
      updatedGs | length sx == 0 = spawnSaucer newGs 
                | otherwise      =             newGs
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
  
  
  
