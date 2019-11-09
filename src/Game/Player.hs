module Game.Player (updatePlayer, postUpdatePlayer) where
  
  import Data.Fixed
  import Class.HasGameObject
  import Game.Object
  import Type.State
  import Type.Object.Player
  import Type.Physics.GameObject
  import Type.IO.Input
  import Physics.Collisions

  updatePlayer :: Float -> GameState -> Player
  updatePlayer t gs@GameState{inGame = igs@InGameState{player = p1, asteroids = as}} = p3
    where
      p2 = updatePlayerControl p1 gs t
      p3 = playerHitAsteroids p2 as

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

  postUpdatePlayer :: Float -> GameState -> GameState
  postUpdatePlayer t gs@GameState{inGame = igs@InGameState{player = p, asteroids = as}} = gs{inGame = igs{
    player = updateCooldown t (wrapOutOfBounds p gs)
  }}

  updateCooldown :: Float -> Player -> Player
  updateCooldown t p = p{cooldown = cool}
    where
      cool
        | cooldown p - t > 0 = cooldown p - t
        | otherwise = 0

  
