module Type.Object.Asteroid where

    data Asteroid = Asteroid {
        loc :: (Float, Float)
        v   :: (Float, Float)
    }