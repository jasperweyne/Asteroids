module Type.Object.Explosion (Explosion(..), makeExplosion) where

  import Graphics.Gloss
  import Class.Rendering.Renderable
  import Type.Rendering.Animation
  import Class.HasGameObject
  import Class.Updateable
  import Rendering.GameObject
  import Type.Physics.GameObject
  
  data Explosion = Explosion {
    obj :: GameObject,
    anim :: Animation
  }

  instance Renderable Explosion where
    render x = renderFactory (render (anim x)) (obj x)
      
  instance HasGameObject Explosion where
    getGameObject = obj
    setGameObject x o = x { obj = o }

  instance Updateable Explosion where
    update x@Explosion{ obj = o, anim = m } f = x { obj = update o f, anim = update m f }

  makeExplosion :: Position -> Animation -> Explosion
  makeExplosion p a = Explosion {
    obj = zeroGameObject {
      pos = p,
      vel = zeroVel,
      rot = 0,
      radius = 25
    },
    anim = a
  }
