module Game.Explosion (updateExplosions, postUpdateExplosions) where
    
  import Data.Maybe
  import Type.Object.Explosion
  import Type.Physics.GameObject
  import Type.State
  import Type.Object.Player as Player
  import Type.IO.Input
  import Game.Object
  import Physics.Collisions
  import Type.Rendering.Animation
  
  updateExplosions :: Float -> GameState -> [Explosion]
  updateExplosions t gs@GameState{inputState = ks, inGame = igs}
    = mapMaybe (updateExplosion t) (explosions igs)

  updateExplosion :: Float -> Explosion -> Maybe Explosion
  updateExplosion t ex@Explosion{anim = an}
    | currenttime an >= 1 = Nothing
    | otherwise           = Just ex

  postUpdateExplosions :: Float -> GameState -> GameState
  postUpdateExplosions t gs@GameState{inGame = igs@InGameState{rockets = rs}} = gs