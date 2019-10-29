module Type.State where
  import Graphics.Gloss
  import Graphics.Gloss.Interface.IO.Game
  import Type.IO.Input
  import Type.Physics.GameObject
  import Type.Object.Player
  import Type.Object.Asteroid
  import Type.Object.Saucer

  --Record input state
  data InputState = InputState {
    mouse :: Position,
    keys :: [GameKeyState]
  }

  data GameState = GameState {
    --Assign different step, event and view functions depending on gamestate (menu/playing/score)
    step :: Float -> GameState -> IO GameState,
    event :: Event -> GameState -> IO GameState,
    view :: GameState -> IO Picture,
    processIO :: GameState -> IO GameState, --multiple functions can be added with a (>>=) notation, used for reading and writing score
    inputState :: InputState,
    inGame :: InGameState
  }

  data InGameState = InGameState {
    player :: Player,
    asteroids :: [Asteroid],
    saucers :: [Saucer],
    score :: Int
  }