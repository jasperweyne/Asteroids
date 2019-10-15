module Type.Project where

  renderFactory :: Picture -> (GameObject -> Picture) --Factory method for render function in player, asteroid, etc.
  queueIO :: GameState -> (GameState -> IO GameState) -> GameState --Queues IO-action over gamestate

  loadPlayerPicture :: GameState -> IO GameState
  loadAsteroidPicture :: GameState -> IO GameState
  loadScoreboard :: GameState -> IO GameState
  saveScoreboard :: (String, Int) -> GameState -> IO GameState

  tickObjects :: [GameObject] -> Float -> [GameObject] --Update objects with deltaTime
  moveObjects :: [GameObject] -> Float -> [GameObject] --Move objects according to velocity and deltaTime

  explode :: Asteroid -> [Asteroid]

  collides :: GameObject -> GameObject -> Bool

  playerHitAsteroids :: Player -> [Asteroid] -> Player --reduce lives
  playerHitSaucers :: Player -> [Saucer] -> Player --reduce lives
  rocketHitAsteroids :: Rocket -> [Asteroid] -> Maybe Rocket
  rocketHitSaucers :: Rocket -> [Saucer] -> Maybe Rocket
  asteroidHitRockets :: Asteroid -> [Rockets] -> [Asteroid]
  saucerHitRockets :: Saucer -> [Rockets] -> Maybe Saucer

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

  data Scoreboard = Scoreboard {
    scores :: [(String, Int)]
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

  --Record input state
  data InputState = InputState {
    mouse :: Position,
    keys :: [KeyState]
  }

  data KeyState = KeyState Key ButtonState
  data ButtonState = Pressed | Released
  data Key = Up | Down | TurnLeft | TurnRight | Shoot | Pause

  --Typed game objects
  data Asteroid = Asteroid {
    obj :: GameObject,
    level :: Int
  }

  data Saucer = Saucer {
    obj :: GameObject
  }

  data Player = Player {
    obj :: GameObject,
    lives :: Int
    render :: GameObject -> Picture
  }

  data Rocket = Rocket {
    obj :: GameObject
  }

  --Generic game object
  data GameObject = GameObject {
    pos :: Position,
    vel :: Velocity,
    rot :: Float,
    radius :: Float
  }

  --Generic data structs
  data Position = Pos Float Float
  data Velocity = Vel Float Float

  move :: Position -> Velocity -> Position