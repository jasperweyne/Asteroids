-- | This module contains the data types
--   which represent the state of the game
module Model where
  import Graphics.Gloss
  import System.Random
  import Type.State
  import Type.IO.Input
  import View.Menu
  import Controller.Menu
  import Type.Object.Player
  import Type.Physics.GameObject
  import Type.Rendering.Animation
  import Rendering.GameObject

  initialGameState :: (Int, Int) -> GameState
  initialGameState screen = GameState {
    mode = Menu,
    processIO = return,
    inputState = InputState 0 [GameKeyState gks Up | gks <- enumFrom Forward] screen,
    inGame = InGameState {
      player = Player {
        obj = zeroGameObject{radius = 25},
        lives = 3,
        picture = blank,
        moving = EmptyAnim
      },
      asteroids = [],
      saucers = [],
      score = 0
    },
    randGen = mkStdGen 0, --Assign getStdGen later through IO action
    asteroidPicture = blank,
    saucerPicture = blank
  }
