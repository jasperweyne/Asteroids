module Type.Object.Player where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Class.Updateable
    import Rendering.GameObject
    import Type.Physics.GameObject
    import Type.Rendering.Animation

    data Player = Player {
        obj :: GameObject,
        lives :: Int,
        picture :: Picture,
        moving :: Animation
    }

    instance Renderable Player where
        render x@Player { obj = o } = renderFactory (transform frame) (obj x)
            where
                transform = (Scale 0.1 0.1) . Rotate (-90)
                frame | acc o > 0 = render (moving x)
                      | otherwise = picture x

    instance Updateable Player where
        update x@Player{ obj = o, moving = m } f = x { obj = update o f, moving = update m f }
