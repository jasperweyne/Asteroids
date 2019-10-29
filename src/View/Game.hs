-- | This module defines how to turn
--   the game state into a picture
module View.Game (viewGame) where

  import Graphics.Gloss
  import Class.Rendering.Renderable
  import Type.Object.Player
  import Type.State

  viewGame :: GameState -> IO Picture
  viewGame = return . renderObjects . inGame
  
  renderObjects :: InGameState -> Picture
  renderObjects gs = Pictures (map render (asteroids gs) ++ map render (saucers gs) ++ [render (player gs)])