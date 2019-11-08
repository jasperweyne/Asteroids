module Game.Object where
  import Data.Fixed
  import Class.HasGameObject
  import Type.State
  import Type.Physics.GameObject
  import Type.IO.Input
  import Physics.Collisions
  import System.Random

  outOfBounds :: GameObject -> GameState -> Either GameObject GameObject
  outOfBounds g GameState{inputState = s} | x >= 0 && x < w && y >= 0 && y < h = Right g
                                          | otherwise                          = Left wrapped
    where
      wrapped = g {
        pos = Pos {
          posX = (x `mod'` w) - w / 2,
          posY = (y `mod'` h) - h / 2
        }
      }
      x = w / 2 + posX (pos g)
      y = h / 2 + posY (pos g)
      w = fromIntegral . fst $ screen s
      h = fromIntegral . snd $ screen s

  removeOutOfBounds :: HasGameObject t => t -> GameState -> Maybe t
  removeOutOfBounds x gs = either (const Nothing) (Just . setGameObject x) (outOfBounds (getGameObject x) gs)

  wrapOutOfBounds :: HasGameObject t => t -> GameState -> t
  wrapOutOfBounds x gs = either (setGameObject x) (setGameObject x) (outOfBounds (getGameObject x) gs)
  
  spawnOnBounds :: Float -> GameState -> IO GameObject
  spawnOnBounds v GameState{inputState = s} = do
    newVel <- genVel
    return zeroGameObject {
      pos = genPos newVel,
      vel = newVel
    }
    where
      getLen x y = sqrt $ x * x + y * y
      genVel = do
        x <- randomRIO (-1, 1)
        y <- randomRIO (-1, 1)
        return $ Vel (x / (getLen x y) * v) (y / (getLen x y) * v)
      genPos (Vel x y) | abs(ex * vy / vx) < ey = Pos ex (ex * vy / vx)
                       | otherwise              = Pos (ey * vx / vy) ey
        where 
          vx = -1 * x / (getLen x y);
          vy = -1 * y / (getLen x y);
          ex = (\e -> e-1) . (/2) . fromIntegral . fst $ screen s
          ey = (\e -> e-1) . (/2) . fromIntegral . snd $ screen s
      
