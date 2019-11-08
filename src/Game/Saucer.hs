module Game.Saucer where

  import Data.Maybe
  import Game.Object
  import Type.State

  postUpdateSaucers :: Float -> GameState -> GameState
  postUpdateSaucers t gs@GameState{inGame = igs@InGameState{saucers = s}} = gs{inGame = igs{
    saucers = mapMaybe (flip removeOutOfBounds $ gs) s
  }}