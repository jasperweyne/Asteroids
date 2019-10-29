module Type.Physics.Body where
  
  data Position = Pos (Float, Float)
  data Velocity = Vel (Float, Float)
  --instance Num Velocity where
  --  Vel (x1, y1) + Vel (x2, y2) = Vel(x1 + x2, y1 + y2)

