module Type.Rendering.Animation where

  import Class.Renderable
  import Class.Updateable
  import Data.Fixed
  import Graphics.Gloss.Data.Picture
  
  data Animation = Animation {
    frames :: [Picture],
    frametime :: Float,
    currenttime :: Float
  } | EmptyAnim

  --Render frames in animation
  instance Renderable Animation where
    render EmptyAnim = blank
    render x = frames x !! floor (frame_idx `mod'` frame_wrap)
      where
        frame_idx  = currenttime x / frametime x
        frame_wrap = (fromIntegral . length) (frames x)

  --Update to next frame
  instance Updateable Animation where
    update EmptyAnim _ = EmptyAnim
    update x@Animation { currenttime = c } f = x { currenttime = c + f }