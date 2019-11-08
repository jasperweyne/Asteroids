module Type.Object.Saucer where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Class.HasGameObject
    import Class.Updateable
    import Rendering.GameObject
    import Type.Physics.GameObject

    data Saucer = Saucer {
        obj :: GameObject,
        picture :: Picture
    }

    instance Renderable Saucer where
        render x = renderFactory (picture x) (obj x)

    instance HasGameObject Saucer where
        getGameObject = obj
        setGameObject x o = x { obj = o }


    instance Updateable Saucer where
        update x@Saucer{ obj = o } f = x { obj = update o f }