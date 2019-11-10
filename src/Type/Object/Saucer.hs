module Type.Object.Saucer where

  import Graphics.Gloss
  import Class.Renderable
  import Class.HasGameObject
  import Class.Updateable
  import Rendering.GameObject
  import Type.Physics.GameObject

  data Saucer = Saucer {
    obj :: GameObject,
    picture :: Picture,
    cooldown :: Float,
    moveCool :: Float
  }

  instance Renderable Saucer where
    render x = renderFactory (picture x) (obj x)

  instance HasGameObject Saucer where
    getGameObject = obj
    setGameObject x o = x { obj = o }

  instance Updateable Saucer where
    update x@Saucer{ obj = o, cooldown = c, moveCool = m } f = x {
      obj = update o f,
      cooldown = max 0 (c - f),
      moveCool = max 0 (m - f)
    }