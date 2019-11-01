module Type.State (InputState, GameMode(..), GameState(..), InGameState(..)) where
  import Class.Updateable
  import Graphics.Gloss
  import Graphics.Gloss.Interface.IO.Game
  import Type.IO.Input
  import Type.Physics.GameObject
  import Type.Object.Player
  import Type.Object.Asteroid
  import Type.Object.Saucer

  data GameMode = Menu | Playing | Score

  data GameState = GameState {
    mode :: GameMode,
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

  instance Updateable InGameState where
    update x f = x {
      player = update (player x) f
    }