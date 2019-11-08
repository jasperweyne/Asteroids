module Game.Object where
  import Data.Fixed
  import Class.HasGameObject
  import Type.State
  import Type.Physics.GameObject
  import Type.IO.Input
  import Physics.Collisions

  outOfBounds :: HasGameObject t => t -> GameState -> Either t t
  outOfBounds g GameState{inputState = s} | x >= 0 && x < w && y >= 0 && y < h = Right g
                                          | otherwise                          = Left wrapped
    where
      wrapped = setGameObject g (getGameObject g) {
        pos = Pos {
          posX = (x `mod'` w) - w / 2,
          posY = (y `mod'` h) - h / 2
        }
      }
      p = pos (getGameObject g)
      x = w / 2 + posX p
      y = h / 2 + posY p
      w = fromIntegral . fst $ screen s
      h = fromIntegral . snd $ screen s

  removeOutOfBounds :: HasGameObject t => t -> GameState -> Maybe t
  removeOutOfBounds x gs = either (const Nothing) Just (outOfBounds x gs)

  wrapOutOfBounds :: HasGameObject t => t -> GameState -> t
  wrapOutOfBounds x gs = either id id (outOfBounds x gs)
                    
