module Type.Physics.GameObject (Position(..), zeroPos, Velocity(..), zeroVel, GameObject(..), zeroGameObject, distance, move, accelDir) where
  import Prelude
  import Class.Updateable

  data GameObject = GameObject {
    pos :: Position,
    vel :: Velocity,
    acc :: Float,
    rot :: Float,
    radius :: Float
  }

  instance Updateable GameObject where
    update o t = o { pos = newPos, vel = newVel }
      where
        newPos = move (pos o) newVel t
        newVel = accelDir (vel o) (rot o) (acc o) t

  zeroGameObject :: GameObject
  zeroGameObject = GameObject {
    pos = zeroPos,
    vel = zeroVel,
    acc = 0,
    rot = 0,
    radius = 0
  }

  --Generic data structs
  data Position = Pos {
    posX :: Float,
    posY :: Float
  }
  
  instance Num Position where
    (Pos x1 y1) + (Pos x2 y2) = Pos (x1 + x2) (y1 + y2)
    (Pos x1 y1) - (Pos x2 y2) = Pos (x1 - x2) (y1 - y2)
    (Pos x1 y1) * (Pos x2 y2) = Pos (x1 * x2) (y1 * y2)

  distance :: Position -> Position -> Float
  distance p1 p2 = sqrt $ x * x + y * y
    where
      (Pos x y) = p2 - p1

  zeroPos :: Position
  zeroPos = Pos {
    posX = 0,
    posY = 0
  }
  
  data Velocity = Vel Float Float

  zeroVel :: Velocity
  zeroVel = Vel 0 0

  move :: Position -> Velocity -> Float -> Position 
  move (Pos px py) (Vel vx vy) t = Pos (px + vx * t) (py + vy * t)

  accelDir :: Velocity -> Float -> Float -> Float -> Velocity
  accelDir (Vel x y) r a t = Vel (x + cos r * a * t) (y - sin r * a * t)