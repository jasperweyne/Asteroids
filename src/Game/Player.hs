module Game.Player (updatePlayer, postUpdatePlayer) where
  import Data.Fixed
  import Game.Object
  import Type.State
  import Type.Object.Player
  import Type.Physics.GameObject
  import Type.IO.Input
  import Physics.Collisions

  updatePlayer :: Float -> GameState -> Player
  updatePlayer t gs@GameState{inGame = igs@InGameState{player = p, asteroids = as}} = playerHitAsteroids (updatePlayerControl p gs t) as

  updatePlayerControl :: Player -> GameState -> Float -> Player
  updatePlayerControl p@Player{obj = o} GameState{inputState = s} t = p {obj = o{rot = newRot, acc = newAcc}, cooldown = cool}
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
        | cooldown p == 0 && keyDown s Shoot = 0.5
        | otherwise = cooldown p 

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

  
