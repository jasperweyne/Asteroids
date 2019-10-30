module Type.Object.Asteroid where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Rendering.GameObject
    import Type.Physics.GameObject

    data Asteroid = Asteroid {
        obj :: GameObject,
        level :: Int,
        picture :: Picture
    }

    instance Renderable Asteroid where
        render x = renderFactory (picture x) (obj x)