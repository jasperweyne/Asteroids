module Rendering.GameObject where
    import Graphics.Gloss

    renderObj :: GameObject -> Picture -> Picture
    renderObj obj = Translate (posX (pos obj)) (posY (pos obj)) . Rotate (rot obj)