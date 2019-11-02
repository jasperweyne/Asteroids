module Type.Object.Asteroid where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Class.Updateable
    import Rendering.GameObject
    import Type.Physics.GameObject

    data Asteroid = Asteroid {
        obj :: GameObject,
        level :: Int,
        picture :: Picture
    }

    instance Renderable Asteroid where
        render x = renderFactory (picture x) (obj x)

    instance Updateable Asteroid where
        update x@Asteroid{obj = o} f = x{obj = update o f}
