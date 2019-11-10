module Game.Saucer where
  
  import Class.HasGameObject
  import Data.Maybe
  import Graphics.Gloss hiding (Vector)
  import Game.Object
  import Game.Player
  import IO.Queue
  import Physics.Collisions
  import Type.Object.Saucer
  import Type.Object.Rocket hiding (obj, picture)
  import Type.Object.Player hiding (obj, picture, cooldown)
  import Type.Object.Asteroid hiding (obj, picture)
  import Type.Object.Explosion hiding (obj, picture)
  import Type.Physics.GameObject
  import Type.State
  import System.Random

  --Update saucer AI, evade incoming rockets, remove saucer if hit
  updateSaucers :: Float -> GameState -> [Saucer]
  updateSaucers t gs@GameState{inGame = igs@InGameState{saucers = s, asteroids = as}} = s4
    where
      doEvade x | moveCool x == 0 = x {moveCool = 0.2} `evade` ((x `findDangerous` asteroids igs) ++ (x `findDangerous` pRockets igs))
                | otherwise       = x
      s1 = map doEvade s
      s2 = mapMaybe (`removeOnCollision` asteroids igs) s1 
      s3 = mapMaybe (`removeOnCollision` pRockets  igs) s2
      s4 = cooldownOnPlayerRespawn s3 igs

  --Make object evade projectiles    
  evade :: HasGameObject x => x -> [Vector] -> x
  evade x [] = x 
  evade x xs = x `setGameObject` (getGameObject x) {
    vel = toVel (safeDir * Vec speed speed)
  }
    where
      speed = mag.velToVec.vel.getGameObject $ x
      safeDir = norm (foldr (+) zeroVec xs) * Vec (-1) (-1)
  
  --Finds directions AI should NOT go to    
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
          closeTo   a b = mag (getOffset a b) < radius a + radius b * 2

  --Applies a cooldown to all saucers when player has respawned
  cooldownOnPlayerRespawn :: [Saucer] -> InGameState -> [Saucer]
  cooldownOnPlayerRespawn sx igs
    | hasPlayerCollided (player igs) igs = map (\x -> x { cooldown = 3 }) sx
    | otherwise = sx 
      
  --Make AI shoot, spawn new saucers, wrap saucers
  postUpdateSaucers :: Float -> GameState -> GameState
  postUpdateSaucers t gs@GameState{inGame = igs@InGameState{saucers = s, explosions = e}} = updatedGs
    where
      updatedGs = attemptSaucerSpawn t.saucersShoot $ newGs
      newGs = gs{inGame = igs{
        saucers = map (`wrapOutOfBounds` gs) s
      }}

  --Attempt saucer spawn using time-adjusted probability
  attemptSaucerSpawn :: Float -> GameState -> GameState
  attemptSaucerSpawn t gs
    | r < p     = (spawnSaucer gs){randGen = g1}
    | otherwise = gs{randGen = g1}
    where
      p = t / 20
      (r, g1) = randomR (0, 1) (randGen gs)
      
  --Spawn a new saucer
  spawnSaucer :: GameState -> GameState
  spawnSaucer gs@GameState{inGame = igs@InGameState{saucers = s}} =
    let (spawn, r) = spawnOnBounds (randGen gs) 100 gs in
    gs{randGen = r, inGame = igs{
      saucers = Saucer {
        obj = spawn {
            radius = 25
        },
        picture = saucerPicture gs,
        cooldown = 1.25,
        moveCool = 0
      } : s
    }}

  --Shoot rocket
  saucersShoot :: GameState -> GameState
  saucersShoot gs@GameState {inGame = igs} = gs{inGame = igs{
    saucers = sx,
    sRockets = catMaybes rx ++ sRockets igs
  }}
    where
      (sx, rx) = unzip (map shoot (saucers igs))
      shoot s | cooldown s == 0 && d < 400 = (s{cooldown = 1}, Just (saucerRocketFor s p px))
              | otherwise                  = (s, Nothing)
        where
          px = rocketPicture gs
          p = player igs
          d = distance (pos.getGameObject $ s) (pos.getGameObject $ p)
  
  
  
