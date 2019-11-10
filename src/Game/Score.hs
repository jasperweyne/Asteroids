module Game.Score (updateScore) where
    
  import Type.Object.Asteroid
  import Type.State
  import Game.Object
  import Physics.Collisions
  import Type.Rendering.Animation
  
  updateScore :: GameState -> Int
  updateScore gs@GameState{inGame = igs} = score igs + collectPoints igs

  collectPoints :: InGameState -> Int
  collectPoints igs@InGameState{asteroids=as, pRockets=pr, saucers=sx, player=p} =
    foldr (+) 0 (map detectPnts pr)
      where
        detectPnts x | x `collidesWith` sx = 200
                     | x `collidesWith` (filter (\x -> level x == 3) as) = 50
                     | x `collidesWith` (filter (\x -> level x  < 3) as) = 20
                     | otherwise           = 0