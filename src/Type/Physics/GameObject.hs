module Type.Physics.GameObject (move, collides) where
  
  data GameObject = GameObject {
    pos :: Position,
    vel :: Velocity,
    rot :: Float,
    radius :: Float
  }

  --Generic data structs
  data Position = Pos Float Float
  data Velocity = Vel Float Float

  move :: Position -> Velocity -> Position 

  collides :: GameObject -> GameObject -> Bool