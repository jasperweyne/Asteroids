module Type.Physics.GameObject (Position(..), zeroPos, Velocity(..), zeroVel, GameObject(..), zeroGameObject, move, accelDir, collides) where
  import Prelude

  data GameObject = GameObject {
    pos :: Position,
    vel :: Velocity,
    rot :: Float,
    radius :: Float
  }

  zeroGameObject :: GameObject
  zeroGameObject = GameObject {
    pos = zeroPos,
    vel = zeroVel,
    rot = 0,
    radius = 0
  }

  --Generic data structs
  data Position = Pos {
    posX :: Float,
    posY :: Float
  }
  
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
  accelDir (Vel x y) r a t = Vel (x + sin r * a * t) (y + cos r * a * t)

  collides :: GameObject -> GameObject -> Bool
  collides g1 g2 = False