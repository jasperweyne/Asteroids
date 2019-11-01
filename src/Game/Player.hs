module Game.Player (updatePlayer, postUpdatePlayer) where
  import Data.Fixed
  import Type.State
  import Type.Object.Player
  import Type.Physics.GameObject
  import Type.IO.Input

  updatePlayer :: Float -> GameState -> GameState
  updatePlayer t gs@GameState{inGame = igs@InGameState{player = p}} = gs{inGame = igs{player = updatePlayerControl p gs t}}

  updatePlayerControl :: Player -> GameState -> Float -> Player
  updatePlayerControl p@Player{obj = o} GameState{inputState = s} t = p {obj = o{rot = newRot, acc = newAcc}}
    where
      newAcc = case keyDown s Forward of
        True -> 100
        False -> 0
      newRot = (rot o) + newRotLeft + newRotRight
      newRotLeft = case keyDown s TurnLeft of
        True -> (-t) * 5
        False -> 0
      newRotRight = case keyDown s TurnRight of
        True -> t * 5
        False -> 0

  postUpdatePlayer :: Float -> GameState -> GameState
  postUpdatePlayer t gs@GameState{inGame = igs@InGameState{player = p}} = gs{inGame = igs{player = wrapPlayer p gs}}

  wrapPlayer :: Player -> GameState -> Player
  wrapPlayer p@Player{obj = o} GameState{inputState = s} = show (x, y) `trace` p {obj = o{pos = Pos{posX = x, posY = y}}}
    where
      x = ((w / 2 + posX (pos o)) `mod'` w) - w / 2
      y = ((h / 2 + posY (pos o)) `mod'` h) - h / 2
      w = fromIntegral . fst $ screen s
      h = fromIntegral . snd $ screen s
