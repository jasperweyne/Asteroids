module Type.Project where

  data Position = Pos (Float, Float)
  data Velocity = Vel (Float, Float)

  tickObject :: DynamicObject -> Float -> DynamicObject
  move :: Position -> Velocity -> Position

  data DynamicObject = DynamicObject {
    pos :: Position,
    vel :: Velocity
  }

  data Saucer = Saucer {
      obj :: DynamicObject
  }

  