module Class.Rendering.Renderable where

  import Graphics.Gloss.Data.Picture
  import Type.Physics.GameObject

  class Renderable a where
    render :: a -> Picture