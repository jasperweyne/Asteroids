module Game.Type.State where

  --Record input state
  data InputState = InputState {
    mouse :: Position,
    keys :: [KeyState]
  }

  data GameState = GameState {
    --Assign different step, event and view functions depending on gamestate (menu/playing/score)
    step :: GameState -> Float -> GameState,
    event :: GameState -> Float -> GameState,
    view :: GameState -> Picture,
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
  
  initialGameState :: GameState
  initialGameState = {
    step = stepMenu,
    event = eventMenu,
    view = viewMenu,
    inputState = ..., --pseudo
    inGame = InGameState {
      player = ..., --pseudo
      asteroids = [],
      saucers = [],
      score = 0
    }
  }