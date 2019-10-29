module Controller.Game (stepGame, eventGame, initialGameState) where

  import Graphics.Gloss.Interface.IO.Game
  import Rendering.GameObject
  import View.Game
  import Type.Physics.GameObject
  import Type.Object.Player
  import Type.State
  
  tickObjects :: [GameObject] -> Float -> [GameObject] --Update objects with deltaTime
  tickObjects = undefined
  
  stepGame :: Float -> GameState -> IO GameState
  stepGame _ = return

  eventGame :: Event -> GameState -> IO GameState
  eventGame _ = return

  initialGameState :: GameState
  initialGameState = GameState {
    step = stepGame,
    event = eventGame,
    view = viewGame,
    inGame = InGameState {
      player = Player {
        obj = zeroGameObject {
          pos = Pos 100 100
        },
        lives = 0,
        renderFn = playerRenderFn
      },
      asteroids = [],
      saucers = [],
      score = 0
    }
  }

  playerRenderFn = renderFactory . color blue $ rectangleSolid 10 10