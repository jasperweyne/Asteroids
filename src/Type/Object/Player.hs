module Type.Object.Player where

  import Graphics.Gloss
  import Class.Renderable
  import Class.HasGameObject
  import Class.Updateable
  import Rendering.GameObject
  import Type.Physics.GameObject
  import Type.Rendering.Animation

  data Player = Player {
    obj :: GameObject,
    lives :: Int,
    picture :: Picture,
    moving :: Animation,
    cooldown :: Float
  }

  instance Renderable Player where
    render x@Player { obj = o } = renderFactory frame (obj x)
      where
        frame | acc o > 0 = render (moving x)
              | otherwise = picture x
                  
  instance HasGameObject Player where
    getGameObject = obj
    setGameObject x o = x { obj = o }

  instance Updateable Player where
    update x@Player{ obj = o, moving = m, cooldown = c } f = x {
        obj = update o f,
        moving = update m f,
        cooldown = max 0 (c - f)
    }
