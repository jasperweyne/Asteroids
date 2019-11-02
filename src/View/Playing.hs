module View.Playing (viewPlaying) where
  import Graphics.Gloss
  import Class.Rendering.Renderable
  import Type.Object.Player
  import Type.State

  viewPlaying :: GameState -> IO Picture
  viewPlaying gs@GameState{inGame = igs} = return $ pictures [renderObjects igs, renderHud igs]
  
  renderObjects :: InGameState -> Picture
  renderObjects gs = Pictures (map render (asteroids gs) ++ map render (saucers gs) ++ [render (player gs)])

  renderHud :: InGameState -> Picture
  renderHud gs@InGameState{player = p@Player{lives = l}, asteroids = as} = translate 350 350 $ color white $ pictures [
      scale 0.1 0.1 $ text ("Lives: " ++ show l),
      translate 0 (-30) $ scale 0.1 0.1 $ text ("Asteroids: " ++ show (length as))
    ] 