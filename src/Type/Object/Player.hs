module Type.Object.Player () where

    import Graphics.Gloss
    import Class.Rendering.Renderable
    import Type.Physics.GameObject

    data Player = Player {
        obj :: GameObject,
        lives :: Int,
        renderFn :: GameObject -> Picture
    }
    
    buildSaucer :: GameObject -> (GameObject -> Picture) -> Saucer

    noLives :: Player -> Int

    instance Renderable Player where
        render x = (renderFn x) (obj x)

    instance GameObject where
        object x = obj x