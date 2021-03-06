module Type.State (InputState, GameMode(..), GameState(..), InGameState(..)) where
  
  import Class.Updateable
  import Graphics.Gloss
  import Graphics.Gloss.Juicy
  import Graphics.Gloss.Interface.IO.Game
  import System.Random
  import Type.Rendering.Animation
  import Type.IO.Input
  import Type.IO.Scoreboard
  import Type.Physics.GameObject
  import Type.Object.Player
  import Type.Object.Explosion
  import Type.Object.Rocket
  import Type.Object.Asteroid
  import Type.Object.Saucer

  data GameMode = Menu | Playing | Paused | Score | Settings deriving (Eq)

  data GameState = GameState {
    mode :: GameMode,
    processIO :: GameState -> IO GameState, --multiple functions can be added with a (>>=) notation, used for reading and writing score
    inputState :: InputState,
    highscores :: Scoreboard,
    inGame :: InGameState,
    randGen :: StdGen,
    rocketPicture :: Picture,
    asteroidPicture :: Picture,
    saucerPicture :: Picture,
    explosion :: Animation
  }

  data InGameState = InGameState {
    player :: Player,
    pRockets :: [Rocket],
    sRockets :: [Rocket],
    asteroids :: [Asteroid],
    saucers :: [Saucer],
    explosions :: [Explosion],
    score :: Int
  }

  --Update objects inside InGameState if it is updated
  instance Updateable InGameState where
    update x f = x {
      player     = update (player x) f,
      pRockets   = (`update` f) <$> pRockets x,
      sRockets   = (`update` f) <$> sRockets x,
      asteroids  = (`update` f) <$> asteroids x,
      saucers    = (`update` f) <$> saucers x,
      explosions = (`update` f) <$> explosions x
    }