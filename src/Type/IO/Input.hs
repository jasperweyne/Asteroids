module Type.IO.Input (InputState(..), GameKeyState(..), ButtonState(..), Key(..)) where
  --Record input state
  data InputState = InputState {
    mouse :: Float,
    keys :: [GameKeyState]
  }

  data GameKeyState = GameKeyState Key ButtonState
  data ButtonState = Pressed | Released deriving (Eq)
  data Key = Up | Down | TurnLeft | TurnRight | Shoot | Pause | Start deriving (Eq, Enum)

  keyDown :: InputState -> Key -> Bool
  keyDown (InputState _ keys) kp = any (\(GameKeyState k state) -> kp == k && state == Pressed) keys
