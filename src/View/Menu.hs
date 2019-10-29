module View.Menu (viewMenu) where
  import Graphics.Gloss.Interface.IO.Game
  import Type.State

  viewMenu :: GameState -> IO Picture
  viewMenu = return . viewMenuPure

  viewMenuPure :: GameState -> Picture
  viewMenuPure gstate = blank