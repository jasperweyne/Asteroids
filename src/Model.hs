-- | This module contains the data types
--   which represent the state of the game
module Model where

  import Type.Object

  data InfoToShow = ShowNothing
                  | ShowANumber Int
                  | ShowAChar   Char

  nO_SECS_BETWEEN_CYCLES :: Float
  nO_SECS_BETWEEN_CYCLES = 5

  data GameState = GameState {
                    infoToShow  :: InfoToShow
                  , elapsedTime :: Float
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

  initialState :: GameState
  initialState = GameState ShowNothing 0

