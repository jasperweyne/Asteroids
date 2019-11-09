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
      updatedGs | length sx == 0 = spawnSaucer newGs 
                | otherwise      =             newGs
      newGs = gs{inGame = igs{
        saucers = sx
      }}
      sx = map (`wrapOutOfBounds` gs) s
      
  spawnSaucer :: GameState -> GameState
  spawnSaucer gs@GameState{inGame = igs@InGameState{saucers = s}} =
    let (spawn, r) = spawnOnBounds (randGen gs) 100 gs in
    gs{randGen = r, inGame = igs{
      saucers = Saucer {
        obj = spawn {
            radius = 25
        },
        picture = saucerPicture gs
      } : s
    }}