module Type.Physics.GameObject (Position(..), zeroPos, Velocity(..), zeroVel, Vector(..), zeroVec, GameObject(..), toPos, mag, norm, toVel, posToVec, velToVec, offset, zeroGameObject, distance, move, accelDir) where
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
  data Vector = Vec {
    axisX :: Float,
    axisY :: Float
  }

  data Position = Pos {
    posX :: Float,
    posY :: Float
  }

  data Velocity = Vel {
    velX :: Float,
    velY :: Float
  }

  instance Num Vector where
    (Vec x1 y1) + (Vec x2 y2) = Vec (x1 + x2) (y1 + y2)
    (Vec x1 y1) - (Vec x2 y2) = Vec (x1 - x2) (y1 - y2)
    (Vec x1 y1) * (Vec x2 y2) = Vec (x1 * x2) (y1 * y2)
  
  instance Num Position where
    (Pos x1 y1) + (Pos x2 y2) = Pos (x1 + x2) (y1 + y2)
    (Pos x1 y1) - (Pos x2 y2) = Pos (x1 - x2) (y1 - y2)
    (Pos x1 y1) * (Pos x2 y2) = Pos (x1 * x2) (y1 * y2)

  instance Num Velocity where
    (Vel x1 y1) + (Vel x2 y2) = Vel (x1 + x2) (y1 + y2)
    (Vel x1 y1) - (Vel x2 y2) = Vel (x1 - x2) (y1 - y2)
    (Vel x1 y1) * (Vel x2 y2) = Vel (x1 * x2) (y1 * y2)

  posToVec :: Position -> Vector
  posToVec (Pos x y) = Vec x y

  velToVec :: Velocity -> Vector
  velToVec (Vel x y) = Vec x y

  toVel :: Vector -> Velocity
  toVel (Vec x y) = Vel x y

  toPos :: Vector -> Position
  toPos (Vec x y) = Pos x y

  mag :: Vector -> Float
  mag (Vec x y) = sqrt (x * x + y * y)

  norm :: Vector -> Vector
  norm v@(Vec x y) = let m = mag v in
    Vec (x / m) (y / m)

  distance :: Position -> Position -> Float
  distance p1 p2 = sqrt $ x * x + y * y
    where
      (Vec x y) = posToVec (p2 - p1)

  offset :: Position -> Position -> Vector
  offset p1 p2 = posToVec (p2 - p1)

  zeroPos :: Position
  zeroPos = Pos {
    posX = 0,
    posY = 0
  }

  zeroVel :: Velocity
  zeroVel = Vel 0 0
  
  zeroVec :: Vector
  zeroVec = Vec 0 0

  move :: Position -> Velocity -> Float -> Position 
  move (Pos px py) (Vel vx vy) t = Pos (px + vx * t) (py + vy * t)

  accelDir :: Velocity -> Float -> Float -> Float -> Velocity
  accelDir (Vel x y) r a t = Vel (x + cos r * a * t) (y - sin r * a * t)