module Class.Rendering.Renderable where

    import Graphics.Gloss.Data.Picture
    import Type.Physics.GameObject

    class Renderable where
        render :: GameObject -> Picture