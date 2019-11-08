module Game.Object where
  import Data.Fixed
  import Class.HasGameObject
  import Type.State
  import Type.Physics.GameObject
  import Type.IO.Input
  import Physics.Collisions

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
                    
