module Type.Physics.GameObject (Position(..), zeroPos, Velocity(..), zeroVel, GameObject(..), zeroGameObject, move, accelDir, collides) where
  import Prelude

  data GameObject = GameObject {
    pos :: Position,
    vel :: Velocity,
    acc :: Float,
    rot :: Float,
    radius :: Float
  }

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
  
  zeroPos :: Position
  zeroPos = Pos {
    posX = 0,
    posY = 0
  }
  
  data Velocity = Vel Float Float

  zeroVel :: Velocity
  zeroVel = Vel 0 0

  move :: Position -> Velocity -> Position 
  move (Pos px py) (Vel vx vy) = Pos (px + vx) (py + vy)

  accelDir :: Velocity -> Float -> Float -> Velocity
  accelDir (Vel x y) r a = Vel (x + sin r * a) (y + cos r * a)

  collides :: GameObject -> GameObject -> Bool
  collides g1 g2 = False