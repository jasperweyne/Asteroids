module Type.IO.Input (GameKeyState, ButtonState, Key) where
  data GameKeyState = KeyState Key ButtonState
  data ButtonState = Pressed | Released
  data Key = Up | Down | TurnLeft | TurnRight | Shoot | Pause
