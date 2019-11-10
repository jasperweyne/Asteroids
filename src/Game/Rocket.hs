module Game.Rocket (updatePlayerRockets, updateSaucerRockets, postUpdateRockets) where
  
  import Data.Maybe
  import Type.Object.Rocket 
  import Type.Physics.GameObject
  import Type.State
  import Type.Object.Player
  import Type.IO.Input
  import Game.Object
  import Physics.Collisions
  
  --Shoot rocket if pressing Shoot, await cooldown
  updatePlayerRockets :: Float -> GameState -> [Rocket]
  updatePlayerRockets t gs@GameState{inputState = ks, rocketPicture = rp, inGame = igs}
    | keyDown ks Shoot && (cooldown.player) igs == 0 = playerRocketFor (player igs) rp : r2
    | otherwise = r2
    where 
      --Remove rockets hitting asteroids
      r1 = mapMaybe (`removeOnCollision` asteroids igs) (pRockets igs)
      r2 = mapMaybe (`removeOnCollision`   saucers igs) r1
  
  --Update rockets shot by saucers
  updateSaucerRockets :: Float -> GameState -> [Rocket]
  updateSaucerRockets t gs@GameState{inputState = ks, rocketPicture = rp, inGame = igs} = s3
    where
      s2 = mapMaybe (`removeOnCollision` asteroids igs)  (sRockets igs)
      s3 = mapMaybe (`removeOnCollision`   [player igs]) s2

  --Remove rockets if outOfBounds
  postUpdateRockets :: Float -> GameState -> GameState
  postUpdateRockets t gs@GameState{inGame = igs@InGameState{pRockets = prs, sRockets = srs}} = gs{inGame = igs{
    pRockets = mapMaybe (`removeOutOfBounds` gs) prs,
    sRockets = mapMaybe (`removeOutOfBounds` gs) srs
  }}