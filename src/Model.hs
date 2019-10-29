-- | This module contains the data types
--   which represent the state of the game
module Model where
  import Graphics.Gloss
  import Type.State
  import View.Menu
  import Controller.Menu
  import Type.Object.Player
  import Type.Physics.GameObject
  import Rendering.GameObject

  data InfoToShow = ShowNothing
                  | ShowANumber Int
                  | ShowAChar   Char

  nO_SECS_BETWEEN_CYCLES :: Float
  nO_SECS_BETWEEN_CYCLES = 5

  initialGameState :: GameState
  initialGameState = GameState {
    mode = Menu,
    processIO = return,
    --inputState = ..., --pseudo
    inGame = InGameState {
      player = Player {
        obj = zeroGameObject,
        picture = blank
      },
      asteroids = [],
      saucers = [],
      score = 0
    }
  }
