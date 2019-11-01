module Class.Updateable where

    class Updateable a where
        update :: a -> Float -> a
