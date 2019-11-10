module Game.Rocket (updatePlayerRockets, updateSaucerRockets, postUpdateRockets) where
  
  import Data.Maybe
  import Type.Object.Rocket 
  import Type.Physics.GameObject
  import Type.State
  import Type.Object.Player
  import Type.IO.Input
  import Game.Object
  import Physics.Collisions
  
  updatePlayerRockets :: Float -> GameState -> [Rocket]
  updatePlayerRockets t gs@GameState{inputState = ks, rocketPicture = rp, inGame = igs}
    | keyDown ks Shoot && (cooldown.player) igs == 0 = (playerRocketFor (player igs) rp) : rs
    | otherwise = rs
    where 
      rs = mapMaybe (`rocketHitAsteroids` asteroids igs) (pRockets igs)
  
  updateSaucerRockets :: Float -> GameState -> [Rocket]
  updateSaucerRockets t gs@GameState{inputState = ks, rocketPicture = rp, inGame = igs} =
    mapMaybe (`rocketHitAsteroids` asteroids igs) (sRockets igs)

  postUpdateRockets :: Float -> GameState -> GameState
  postUpdateRockets t gs@GameState{inGame = igs@InGameState{pRockets = prs, sRockets = srs}} = gs{inGame = igs{
    pRockets = mapMaybe (`removeOutOfBounds` gs) prs,
    sRockets = mapMaybe (`removeOutOfBounds` gs) srs
  }}