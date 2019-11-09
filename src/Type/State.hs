module Type.State (InputState, GameMode(..), GameState(..), InGameState(..)) where
  import Class.Updateable
  import Graphics.Gloss
  import Graphics.Gloss.Interface.IO.Game
  import System.Random
  import Type.IO.Input
  import Type.Physics.GameObject
  import Type.Object.Player
  import Type.Object.Rocket
  import Type.Object.Asteroid
  import Type.Object.Saucer

  data GameMode = Menu | Playing | Score

  data GameState = GameState {
    mode :: GameMode,
    processIO :: GameState -> IO GameState, --multiple functions can be added with a (>>=) notation, used for reading and writing score
    inputState :: InputState,
    inGame :: InGameState,
    randGen :: StdGen,
    rocketPicture :: Picture,
    asteroidPicture :: Picture,
    saucerPicture :: Picture
  }

  data InGameState = InGameState {
    player :: Player,
    rockets :: [Rocket],
    asteroids :: [Asteroid],
    saucers :: [Saucer],
    score :: Int
  }

  instance Updateable InGameState where
    update x f = x {
      player    = update (player x) f,
      rockets   = (`update` f) <$> rockets x,
      asteroids = (`update` f) <$> asteroids x,
      saucers   = (`update` f) <$> saucers x
    }