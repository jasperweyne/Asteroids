module Type.IO.Input (InputState(..), InputMode(..), GameKeyState(..), GameKey(..), KeyState(..), keyDown) where
  
  import Graphics.Gloss.Interface.IO.Game
  
  --Record input state
  data InputState = InputState {
    inputmode :: InputMode,
    mouse :: Float,
    keys :: [GameKeyState],
    screen :: (Int, Int)
  }
  
  data InputMode = Keyboard | Mouse deriving (Eq, Enum, Show)
  data GameKeyState = GameKeyState GameKey KeyState
  data GameKey = Forward | Backward | TurnLeft | TurnRight | Shoot | Pause | Start deriving (Eq, Enum)

  keyDown :: InputState -> GameKey -> Bool
  keyDown (InputState _ _ keys _) kp = any (\(GameKeyState gk state) -> kp == gk && state == Down) keys
