module Game.Object where
  
  import Data.Fixed
  import Class.HasGameObject
  import Type.State
  import Type.Physics.GameObject
  import Type.IO.Input
  import Physics.Collisions
  import System.Random

  --Check if gameobject is outOfBounds, wrap around screen
  outOfBounds :: GameObject -> GameState -> Float -> Either GameObject GameObject
  outOfBounds g GameState{inputState = s} off 
    | x >= (-r) && 
      x < (w + r) && 
      y >= (-r) && 
      y < (h + r) = Right g
    | otherwise         = Left wrapped
    where
      wrapped = g {
        pos = Pos {
          posX = (x `mod'` w) - w / 2,
          posY = (y `mod'` h) - h / 2
        }
      }
      x = w / 2 + posX (pos g)
      y = h / 2 + posY (pos g)
      w = 2 * off + (fromIntegral . fst $ screen s)
      h = 2 * off + (fromIntegral . snd $ screen s)
      r = radius g

  --Remove object if outOfBounds
  removeOutOfBounds :: HasGameObject t => t -> GameState -> Maybe t
  removeOutOfBounds x gs = either (const Nothing) (Just . setGameObject x) (outOfBounds (getGameObject x) gs (radius . getGameObject $ x))

  --Apply wrapping if object is outOfBounds
  wrapOutOfBounds :: HasGameObject t => t -> GameState -> t
  wrapOutOfBounds x gs = either (setGameObject x) (setGameObject x) (outOfBounds (getGameObject x) gs (radius . getGameObject $ x))    
  
  --Create random object position and velocity (position on screen bounds, velocity pointing inward)
  spawnOnBounds :: RandomGen g => g -> Float -> GameState -> (GameObject, g)
  spawnOnBounds g1 speed GameState{inputState = s} = (zeroGameObject { pos = Pos px py, vel = toVel (Vec speed speed * norm v) }, g4)
    where
      w = 100 + (fromIntegral . fst $ screen s) --Spawn just beyond border
      h = 100 + (fromIntegral . snd $ screen s)
      (pt, g2) = randomR (0, 2 * w + 2 * h) g1
      (px, py)
        | pt < w         = (pt, h * (-0.5))
        | pt < w * 2     = (pt - w, h * 0.5)
        | pt < w * 2 + h = (w * (-0.5), pt - w * 2)
        | otherwise      = (w * 0.5, pt - (w * 2 + h))
      (tx, g3) = randomR (w * (-0.25), w * 0.25) g2
      (ty, g4) = randomR (h * (-0.25), h * 0.25) g3 
      v = Vec (tx - px) (ty - py)
