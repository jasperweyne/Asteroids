module Game.Player (updatePlayerControl) where
  import Type.Object.Player
  import Type.IO.Input

  updatePlayerControl :: Player -> GameState -> Player
  updatePlayerControl p GameState{inputState = s} = case keyDown s Up of
    True -> accelDir (vel o) (dir o) 5
    False -> vel o
    where
      o = obj p