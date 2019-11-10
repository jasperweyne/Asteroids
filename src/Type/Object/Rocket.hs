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

  playerRocketFor :: HasGameObject g => g -> Picture -> Rocket
  playerRocketFor x p = newPlayerRocket p `rocketFor` x
  
  saucerRocketFor :: HasGameObject g => g -> Picture -> Rocket
  saucerRocketFor x p = newSaucerRocket p `rocketFor` x

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
      r = rot o + pi / 2
      s = radius o
      newPos = Pos (posX p + sin r * s) (posY p + cos r * s)
      newVel = Vel (sin r * 300) (cos r * 300) + vel o
