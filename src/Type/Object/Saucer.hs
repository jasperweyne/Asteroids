module Type.Object.Saucer where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Rendering.GameObject
    import Type.Physics.GameObject

    data Saucer = Saucer {
        obj :: GameObject,
        picture :: Picture
    }

    instance Renderable Saucer where
        render x = renderFactory (picture x) (obj x)
