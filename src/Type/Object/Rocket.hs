module Type.Object.Rocket where

  import Graphics.Gloss
  import Class.Rendering.Renderable
  import Class.HasGameObject
  import Class.Updateable
  import Rendering.GameObject
  import Type.Physics.GameObject
  
  data Rocket = PlayerRocket {
    obj :: GameObject,
    picture :: Picture
  } | SaucerRocket {
    obj :: GameObject,
    picture :: Picture
  }

  instance Renderable Rocket where
    render x = renderFactory (picture x) (obj x)
      
  instance HasGameObject Rocket where
    getGameObject = obj
    setGameObject x o = x { obj = o }

  instance Updateable Rocket where
    update x f = x{obj = update (obj x) f}

  getPlayerRockets :: [Rocket] -> [Rocket]
  getPlayerRockets [] = []
  getPlayerRockets (x@PlayerRocket {} : xs) = x : getPlayerRockets xs
  getPlayerRockets (_                 : xs) =     getPlayerRockets xs
  
  getSaucerRockets :: [Rocket] -> [Rocket]
  getSaucerRockets [] = []
  getSaucerRockets (x@SaucerRocket {} : xs) = x : getPlayerRockets xs
  getSaucerRockets (_                 : xs) =     getPlayerRockets xs

  playerRocketFor :: HasGameObject g => g -> Picture -> Rocket
  playerRocketFor x p = newPlayerRocket p `rocketFor` x
  
  saucerRocketFor :: (HasGameObject x, HasGameObject y) => x -> y -> Picture -> Rocket
  saucerRocketFor x y p = newSaucerRocket p `rocketTo` x $ y

  newPlayerRocket :: Picture -> Rocket
  newPlayerRocket p = PlayerRocket { obj = zeroGameObject { radius = 10 }, picture = p }

  newSaucerRocket :: Picture -> Rocket
  newSaucerRocket p = SaucerRocket { obj = zeroGameObject { radius = 10 }, picture = p }

  rocketFor :: HasGameObject g => Rocket -> g -> Rocket
  rocketFor rx x = rx { obj = (obj rx) {
    pos = newPos,
    vel = newVel,
    rot = r 
  }}
    where
      o = getGameObject x
      p = pos o
      r = rot o
      s = radius o
      newPos = Pos (posX p + cos r * s) (posY p + sin r * s * (-1))
      newVel = Vel (cos r * 300) (sin r * 300 * (-1)) + vel o

  rocketTo :: (HasGameObject x, HasGameObject y) => Rocket -> x -> y -> Rocket
  rocketTo rx x y = rx { obj = (obj rx) {
    pos = newPos,
    vel = newVel,
    rot = r 
  }}
    where
      o = getGameObject x
      p = pos o
      d = offset (pos o) (pos . getGameObject $ y)
      r = (-1) * atan2 (axisY d) (axisX d)
      s = radius o
      newPos = Pos (posX p + cos r * s) (posY p + sin r * s * (-1))
      newVel = Vel (cos r * 300) (sin r * 300 * (-1))
