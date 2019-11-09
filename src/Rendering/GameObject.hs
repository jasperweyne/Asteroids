module Rendering.GameObject where

  import Prelude
  import Graphics.Gloss
  import Class.Rendering.Renderable
  import Type.Physics.GameObject
  
  --Factory method for render function in player, asteroid, etc.
  renderFactory :: Picture -> (GameObject -> Picture)
  renderFactory = flip fn
    where fn GameObject { pos = (Pos px py), rot = r } = Translate px py . Rotate ((r / pi) * 180)