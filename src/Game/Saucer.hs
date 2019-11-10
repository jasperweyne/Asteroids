module Game.Saucer where
  
  import Class.HasGameObject
  import Data.Maybe
  import Graphics.Gloss hiding (Vector)
  import Game.Object
  import IO.Queue
  import Physics.Collisions
  import Type.Object.Saucer
  import Type.Object.Rocket hiding (obj, picture)
  import Type.Object.Player hiding (obj, picture, cooldown)
  import Type.Object.Asteroid hiding (obj, picture)
  import Type.Object.Explosion hiding (obj, picture)
  import Type.Physics.GameObject
  import Type.State

  updateSaucers :: Float -> GameState -> [Saucer]
  updateSaucers t gs@GameState{inGame = igs@InGameState{saucers = s, asteroids = as}} = map doEvade s
    where
      doEvade x = x `evade` concat [x `findDangerous` asteroids igs]

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
      isDangerous y | gx `closeTo` gy = Just (getOffset gx gy)
                    | otherwise       = Nothing
        where
          gx = getGameObject x
          gy = getGameObject y
          getOffset a b = offset (pos a) (pos b)
          closeTo   a b = mag (getOffset a b) < (radius a) * 2 + (radius b)

  postUpdateSaucers :: Float -> GameState -> GameState
  postUpdateSaucers t gs@GameState{inGame = igs@InGameState{saucers = s, explosions = e}} = updatedGs
    where
      updatedGs | null s3   = spawnSaucer.saucersShoot $ newGs 
                | otherwise =             saucersShoot $ newGs
      newGs = gs{inGame = igs{
        saucers = s3,
        explosions = e1 ++ e2 ++ e
      }}
      s1 = map (`wrapOutOfBounds` gs) s
      (e1, s2) = explodeOnCollision (asteroids igs) (explosion gs) $ s1 
      (e2, s3) = explodeOnCollision (rockets   igs) (explosion gs) $ s2
      
  spawnSaucer :: GameState -> GameState
  spawnSaucer gs@GameState{inGame = igs@InGameState{saucers = s}} =
    let (spawn, r) = spawnOnBounds (randGen gs) 100 gs in
    gs{randGen = r, inGame = igs{
      saucers = Saucer {
        obj = spawn {
            radius = 25
        },
        picture = saucerPicture gs,
        cooldown = 0
      } : s
    }}

  saucersShoot :: GameState -> GameState
  saucersShoot gs@GameState {inGame = igs} = gs{inGame = igs{
    saucers = sx,
    rockets = catMaybes rx ++ rockets igs
  }}
    where
      (sx, rx) = unzip (map shoot (saucers igs))
      shoot s | cooldown s == 0 && d < 400 = (s{cooldown = 1}, Just (saucerRocketFor s p px))
              | otherwise                  = (s, Nothing)
        where
          px = rocketPicture gs
          p = player igs
          d = distance (pos.getGameObject $ s) (pos.getGameObject $ p)
  
  
  
