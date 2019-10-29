module View.Playing (viewPlaying) where
  import Graphics.Gloss.Interface.IO.Game
  import Type.State

  viewPlaying :: GameState -> IO Picture
  viewPlaying = return . viewPlayingPure

  viewPlayingPure :: GameState -> Picture
  viewPlayingPure gstate = translate (-250) 0 $ color white $ pictures 
    [
      text "Playing the game"
    ]