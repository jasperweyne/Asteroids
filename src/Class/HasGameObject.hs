module Class.HasGameObject where

    import Type.Physics.GameObject

    class HasGameObject a where
        getGameObject :: a -> GameObject
        setGameObject :: a -> GameObject -> a
