module View.Playing (viewPlaying) where
  
  import Graphics.Gloss
  import Class.Renderable
  import Type.Object.Player
  import Type.State

  --Draw playing screen
  viewPlaying :: GameState -> IO Picture
  viewPlaying gs@GameState{inGame = igs} = return $ pictures [renderObjects igs, renderHud igs]
  
  --Draw all objects in the InGameState
  renderObjects :: InGameState -> Picture
  renderObjects gs = Pictures (map render (asteroids gs) ++ 
                               map render (saucers gs) ++
                               map render (pRockets gs) ++
                               map render (sRockets gs) ++
                               map render (explosions gs) ++
                               [render (player gs)])

  --Draw HUD on screen
  renderHud :: InGameState -> Picture
  renderHud gs@InGameState{player = p@Player{lives = l}, score = s} = translate 350 350 $ color white $ pictures [
                          scale 0.1 0.1 $ text ("Lives: " ++ show l),
      translate 0 (-30) $ scale 0.1 0.1 $ text ("Score: " ++ show s)
    ] 