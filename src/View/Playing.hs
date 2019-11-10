module View.Playing (viewPlaying) where
  
  import Graphics.Gloss
  import Class.Rendering.Renderable
  import Type.Object.Player
  import Type.State

  viewPlaying :: GameState -> IO Picture
  viewPlaying gs@GameState{inGame = igs} = return $ pictures [renderObjects igs, renderHud igs]
  
  renderObjects :: InGameState -> Picture
  renderObjects gs = Pictures (map render (asteroids gs) ++ 
                               map render (saucers gs) ++
                               map render (pRockets gs) ++
                               map render (sRockets gs) ++
                               map render (explosions gs) ++
                               [render (player gs)])

  renderHud :: InGameState -> Picture
  renderHud gs@InGameState{player = p@Player{lives = l}, explosions = ex, asteroids = as, saucers = s, pRockets = pr, sRockets = sr} = translate 350 350 $ color white $ pictures [
      scale 0.1 0.1 $ text ("Lives: " ++ show l),
      translate 0 (-30) $ scale 0.1 0.1 $ text ("Asteroids: " ++ show (length as)),
      translate 0 (-60) $ scale 0.1 0.1 $ text ("Saucers: " ++ show (length s)),
      translate 0 (-90) $ scale 0.1 0.1 $ text ("Rockets: " ++ show (length pr + length sr)),
      translate 0 (-120) $ scale 0.1 0.1 $ text ("Explosions: " ++ show (length ex)),
      translate 0 (-150) $ scale 0.1 0.1 $ text ("Score: " ++ show (score gs))
    ] 