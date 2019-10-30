module Type.Object.Player where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Rendering.GameObject
    import Type.Physics.GameObject

    data Player = Player {
        obj :: GameObject,
        lives :: Int,
        picture :: Picture
    }

    instance Renderable Player where
        render x = renderFactory (picture x) (obj x)
