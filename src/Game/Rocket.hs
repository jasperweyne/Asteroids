module Game.Rocket (updateRockets, postUpdateRockets) where
  
  import Data.Maybe
  import Type.Object.Rocket 
  import Type.Physics.GameObject
  import Type.State
  import Type.Object.Player
  import Type.IO.Input
  import Game.Object
  import Physics.Collisions
  
  updateRockets :: Float -> GameState -> [Rocket]
  updateRockets t gs@GameState{inputState = ks, rocketPicture = rp, inGame = igs}
    | keyDown ks Shoot && (cooldown.player) igs == 0 = (playerRocketFor (player igs) rp) : rs
    | otherwise = rs
    where 
      rs = mapMaybe (`rocketHitAsteroids` asteroids igs) (rockets igs)

  postUpdateRockets :: Float -> GameState -> GameState
  postUpdateRockets t gs@GameState{inGame = igs@InGameState{rockets = rs}} = gs{inGame = igs{
    rockets = mapMaybe (`removeOutOfBounds` gs) rs
  }}