module Class.Rendering.Animatable where

    import Graphics.Gloss.Data.Picture

    class (Drawable) => Animatable where
        animation_index :: Integer
        animation       :: [Picture]