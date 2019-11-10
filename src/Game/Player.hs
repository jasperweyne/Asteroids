module Game.Player (updatePlayer, postUpdatePlayer) where
  
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

  updatePlayer :: Float -> GameState -> Player
  updatePlayer t gs@GameState{inGame = igs@InGameState{player = p1, asteroids = as, sRockets = sx}} = p2
    where
      p2 = updatePlayerControl p1 gs t

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
  postUpdatePlayer t gs@GameState{inGame = igs@InGameState{player = p, asteroids = as, sRockets = sx}} = gs{inGame = igs{
    player = p4
  }}
    where
      p2 = wrapOutOfBounds p gs
      p3 = playerHitAsteroids p2 as
      p4 = playerHitRockets p3 sx as

  playerHitAsteroids :: Player -> [Asteroid] -> Player --reduce lives
  playerHitAsteroids p ast
    | p `collidesWith` ast = respawnPlayer p ast
    | otherwise = p

  playerHitRockets :: Player -> [Rocket] -> [Asteroid] -> Player
  playerHitRockets p rx ast
    | p `collidesWith` rx = respawnPlayer p ast
    | otherwise = p

  respawnPlayer :: Player -> [Asteroid] -> Player
  respawnPlayer p@Player{obj = o, lives = l} ast = p{lives = newLives, obj = newPlyObj}
    where
      newLives = l - 1
      newPlyObj = foldl (\x y -> if collides x y then 
          x{pos = pos x + toPos (offset (pos y) (pos x))} 
        else x) o{pos = Pos 0 0, vel = Vel 0 0} asObjs 
      asObjs = getGameObject <$> ast
      
  --Create explosion if player lost life
  -- ex2  | lives p2 < lives p1 = makeExplosion ((pos.getGameObject) p1) (explosion gs) : ex1 
  --      | otherwise = ex1