module Type.IO.Input where
  data KeyState = KeyState Key ButtonState
  data ButtonState = Pressed | Released
  data Key = Up | Down | TurnLeft | TurnRight | Shoot | Pause
