module Game.Player (updatePlayer, postUpdatePlayer, hasPlayerCollided) where
  
  import Data.Fixed
  import Class.HasGameObject
  import Game.Object
  import Type.State
  import Type.Object.Player
  import Type.Object.Rocket hiding (obj)
  import Type.Object.Asteroid hiding (obj)
  import Type.Physics.GameObject
  import Type.IO.Input
  import Physics.Collisions

  --Update player object
  updatePlayer :: Float -> GameState -> Player
  updatePlayer t gs@GameState{inGame = igs@InGameState{player = p1, asteroids = as}} = p3
    where
      p2 = updatePlayerControl p1 gs t
      p3 = respawnOnPlayerHit p2 igs

  --Check if player has collided with anything
  hasPlayerCollided :: Player -> InGameState -> Bool
  hasPlayerCollided p igs@InGameState{sRockets = rs, asteroids = as} = p `collidesWith` rs || p `collidesWith` as
  
  --When player is hit, respawn
  respawnOnPlayerHit :: Player -> InGameState -> Player
  respawnOnPlayerHit p igs
    | hasPlayerCollided p igs = respawnPlayer p (asteroids igs)
    | otherwise               = p

  --Respawn player
  respawnPlayer :: Player -> [Asteroid] -> Player
  respawnPlayer p@Player{obj = o, lives = l} ast = p{lives = newLives, obj = newPlyObj}
    where
      newLives = l - 1
      --Fold player spawn position over all asteroids, shift player away if colliding
      newPlyObj = foldl (\x y -> if collides x y then 
          x{pos = pos x + toPos (offset (pos y) (pos x))} 
        else x) o{pos = Pos 0 0, vel = Vel 0 0} asObjs 
      asObjs = getGameObject <$> ast

  --Apply rotation and acceleration using player input
  updatePlayerControl :: Player -> GameState -> Float -> Player
  updatePlayerControl p1@Player{obj = o} GameState{inputState = s} t = p2
    where
      newAcc
        | keyDown s Forward = 200
        | otherwise = 0
      newRot = rot o + newRotLeft + newRotRight
      newRotLeft 
        | keyDown s TurnLeft = (-t) * 5
        | otherwise = 0
      newRotRight
        | keyDown s TurnRight = t * 5
        | otherwise = 0
      cool
        | cooldown p1 == 0 && keyDown s Shoot = 0.5
        | otherwise = cooldown p1 
      p2 = p1 {
        obj = o {
          rot = newRot, 
          acc = newAcc
        }, 
        cooldown = cool
      }

  --Wrap player if out of bounds
  postUpdatePlayer :: Float -> GameState -> GameState
  postUpdatePlayer t gs@GameState{inGame = igs@InGameState{player = p, asteroids = as, sRockets = sx}} = gs{inGame = igs{
    player = p2
  }}
    where
      p2 = wrapOutOfBounds p gs
