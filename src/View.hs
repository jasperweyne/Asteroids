-- | This module defines how to turn
--   the game state into a picture
module View where

  import Graphics.Gloss
  import Type.State

  view :: GameState -> Picture
  view = return . viewPure . inGame

  viewPure :: InGameState -> Picture
  viewPure gs = Pictures (map renderObj (asteroids gs) ++ map renderObj (saucers gs) ++ [renderObj (player gs)]

  --renderObj :: Renderable => a -> Picture
  --renderObj x = (render x) (obj x)