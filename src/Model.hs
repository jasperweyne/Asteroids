-- | This module contains the data types
--   which represent the state of the game
module Model where
  
  import Graphics.Gloss
  import System.Random
  import Type.State
  import Type.IO.Input
  import Type.IO.Scoreboard
  import View.Menu
  import Controller.Menu
  import Type.Object.Player
  import Type.Physics.GameObject
  import Type.Rendering.Animation
  import Rendering.GameObject

  --Initial (empty) GameState
  initialGameState :: (Int, Int) -> GameState
  initialGameState screen = GameState {
    mode = Menu,
    processIO = return,
    inputState = InputState Keyboard (0, 0) [GameKeyState gks Up | gks <- enumFrom Forward] screen, --Set empty GameKeyStates
    highscores = Scoreboard {scores = []},
    inGame = InGameState {
      player = Player {
        obj = zeroGameObject{radius = 25},
        lives = 3,
        picture = blank,
        moving = EmptyAnim,
        cooldown = 0
      },
      explosions = [],
      pRockets = [],
      sRockets = [],
      asteroids = [],
      saucers = [],
      score = 0
    },
    randGen = mkStdGen 0, --Assign getStdGen later through IO action
    rocketPicture = blank,
    asteroidPicture = blank,
    saucerPicture = blank,
    explosion = EmptyAnim
  }
