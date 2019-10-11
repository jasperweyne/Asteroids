module Type.Object.Player where

    data Player = Player {
        dir :: Float
        loc :: (Float, Float)
        v   :: (Float, Float)
    }