module Class.HasGameObject where

    import Type.Physics.GameObject

    class HasGameObject a where
        get_gameobject :: a -> GameObject
        set_gameobject :: a -> GameObject -> a
