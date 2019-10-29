module View.Playing (viewPlaying) where
  import Graphics.Gloss
  import Class.Rendering.Renderable
  import Type.Object.Player
  import Type.State

  viewPlaying :: GameState -> IO Picture
  viewPlaying = return . renderObjects . inGame
  
  renderObjects :: InGameState -> Picture
  renderObjects gs = Pictures (map render (asteroids gs) ++ map render (saucers gs) ++ [render (player gs)])