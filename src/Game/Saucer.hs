module Game.Saucer where
  import Debug.Trace
  import Data.Maybe
  import Game.Object
  import IO.Queue
  import Type.Object.Saucer
  import Type.Physics.GameObject
  import Type.State

  postUpdateSaucers :: Float -> GameState -> GameState
  postUpdateSaucers t gs@GameState{inGame = igs@InGameState{saucers = s}} = updatedGs
    where
      updatedGs | length removedSaucers == 0 = queueIO newGs spawnSaucer
                | otherwise                  =         newGs
      newGs = gs{inGame = igs{
        saucers = removedSaucers
      }}
      removedSaucers = mapMaybe (flip removeOutOfBounds $ gs) s


  spawnSaucer :: GameState -> IO GameState
  spawnSaucer gs@GameState{inGame = igs@InGameState{saucers = s}} = return newGs 
    where
      newGs = gs{inGame = igs{
        saucers = Saucer {
          obj = zeroGameObject {
              radius = 25,
              vel = Vel 45 25
          },
          picture = saucerPicture gs
        } : s
      }}