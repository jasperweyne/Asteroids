-- | This module contains the data types
--   which represent the state of the game
module Model where
  import Type.State
  import View.Menu
  import Controller.Menu

  data InfoToShow = ShowNothing
                  | ShowANumber Int
                  | ShowAChar   Char

  nO_SECS_BETWEEN_CYCLES :: Float
  nO_SECS_BETWEEN_CYCLES = 5

  --data GameState = GameState {
  --                 infoToShow  :: InfoToShow
  --               , elapsedTime :: Float
  --               }

  --initialState :: GameState
  --initialState = GameState ShowNothing 0

  initialGameState :: GameState
  initialGameState = GameState {
    step = stepMenu,
    event = eventMenu,
    view = viewMenu,
    --inputState = ..., --pseudo
    inGame = InGameState {
      --player = ..., --pseudo
      asteroids = [],
      saucers = [],
      score = 0
    }
  }
