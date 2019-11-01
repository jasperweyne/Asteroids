module Type.IO.Input (InputState(..), GameKeyState(..), GameKey(..), KeyState(..), keyDown) where
  import Graphics.Gloss.Interface.IO.Game
  --Record input state
  data InputState = InputState {
    mouse :: Float,
    keys :: [GameKeyState]
  }

  data GameKeyState = GameKeyState GameKey KeyState
  data GameKey = Forward | Backward | TurnLeft | TurnRight | Shoot | Pause | Start deriving (Eq, Enum)

  keyDown :: InputState -> GameKey -> Bool
  keyDown (InputState _ keys) kp = any (\(GameKeyState gk state) -> kp == gk && state == Down) keys
