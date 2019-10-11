module Type.Object.Projectile where

    data Projectile = Projectile {
        loc :: (Float, Float)
        v   :: (Float, Float)
    }