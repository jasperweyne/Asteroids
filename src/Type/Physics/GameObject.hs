module Type.Physics.GameObject (Position, Velocity, GameObject, move, collides) where
  
  data GameObject = GameObject {
    pos :: Position,
    vel :: Velocity,
    rot :: Float,
    radius :: Float
  }

  --Generic data structs
  data Position = Pos {
    posX :: Float,
    posY :: Float
  }
  
  data Velocity = Vel Float Float

  move :: Position -> Velocity -> Position 
  move (Pos px py) (Vel vx vy) = Pos (px + vx) (py + vy)

  collides :: GameObject -> GameObject -> Bool
  collides g1 g2 = False