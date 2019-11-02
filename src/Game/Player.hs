module Game.Player (updatePlayer, postUpdatePlayer) where
  import Data.Fixed
  import Type.State
  import Type.Object.Player
  import Type.Physics.GameObject
  import Type.IO.Input
  import Debug.Trace

  updatePlayer :: Float -> GameState -> GameState
  updatePlayer t gs@GameState{inGame = igs@InGameState{player = p}} = gs{inGame = igs{player = updatePlayerControl p gs t}}

  updatePlayerControl :: Player -> GameState -> Float -> Player
  updatePlayerControl p@Player{obj = o} GameState{inputState = s} t = p {obj = o{rot = newRot, acc = newAcc}}
    where
      newAcc
        | keyDown s Forward = 200
        | otherwise = 0
      newRot = rot o + newRotLeft + newRotRight
      newRotLeft 
        | keyDown s TurnLeft = (-t) * 5
        | otherwise = 0
      newRotRight
        | keyDown s TurnRight = t * 5
        | otherwise = 0

  postUpdatePlayer :: Float -> GameState -> GameState
  postUpdatePlayer t gs@GameState{inGame = igs@InGameState{player = p}} = gs{inGame = igs{player = wrapPlayer p gs}}

  wrapPlayer :: Player -> GameState -> Player
  wrapPlayer p@Player{obj = o} GameState{inputState = s} = show (x, y) `trace` p {obj = o{pos = Pos{posX = x, posY = y}}}
    where
      x = ((w / 2 + posX (pos o)) `mod'` w) - w / 2
      y = ((h / 2 + posY (pos o)) `mod'` h) - h / 2
      w = fromIntegral . fst $ screen s
      h = fromIntegral . snd $ screen s
