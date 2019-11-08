module Type.Object.Saucer where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Class.HasGameObject
    import Rendering.GameObject
    import Type.Physics.GameObject

    data Saucer = Saucer {
        obj :: GameObject,
        picture :: Picture
    }

    instance Renderable Saucer where
        render x = renderFactory (picture x) (obj x)

    instance HasGameObject Saucer where
        get_gameobject = obj
        set_gameobject x o = x { obj = o }
