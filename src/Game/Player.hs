module Game.Player (updatePlayer) where
  import Type.State
  import Type.Object.Player
  import Type.Physics.GameObject
  import Type.IO.Input

  updatePlayer :: Float -> GameState -> GameState
  updatePlayer t gs@GameState{inGame = igs@InGameState{player = p}} = gs{inGame = igs{player = updatePlayerControl p gs t}}

  updatePlayerControl :: Player -> GameState -> Float -> Player
  updatePlayerControl p@Player{obj = o} GameState{inputState = s} t = p {obj = o{rot = newRot, vel = newVel}}
    where
      newVel = case keyDown s Forward of
        True -> accelDir (vel o) (rot o) 100 t
        False -> vel o
      newRot = (rot o) + newRotLeft + newRotRight
      newRotLeft = case keyDown s TurnLeft of
        True -> (-t) * 5
        False -> 0
      newRotRight = case keyDown s TurnRight of
        True -> t * 5
        False -> 0