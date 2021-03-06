-- | This module defines how the state changes
--   in response to time and user input
module Controller (step, input) where

  import Model

  import Graphics.Gloss
  import Graphics.Gloss.Interface.IO.Game
  import Type.State
  import Type.IO.Input
  import Controller.Menu
  import Controller.Playing
  import Controller.Paused
  import Controller.Score
  import Controller.Settings

  --Switch step function depending on current mode
  step :: Float -> GameState -> IO GameState
  step t gs@GameState{mode = m} = resetKeyState =<< execQueuedIO =<< 
    (\tgs -> (case m of
    Menu -> stepMenu t tgs
    Playing -> stepPlaying t tgs
    Paused -> stepPaused t tgs
    Score -> stepScore t tgs
    Settings -> stepSettings t tgs)) =<< doKeyState gs
  
  --Update keyboard and mouse state in InputState
  doKeyState :: GameState -> IO GameState
  doKeyState gs@GameState { inputState = is }= case inputmode is of
    Keyboard -> return gs
    Mouse -> setMouse (fst . mouse $ is)
      where
        w = (fromIntegral . fst . screen $ is) / 2
        setMouse x | x >        w / 2 = alterKeyState gs TurnRight Down
                   | x < (-1) * w / 2 = alterKeyState gs TurnLeft  Down
                   | otherwise        = return gs

  --Set InputState to default
  resetKeyState :: GameState -> IO GameState
  resetKeyState gs@GameState { inputState = is }= case inputmode is of
    Keyboard -> return gs
    Mouse -> do 
      x <- alterKeyState gs TurnLeft Up
      alterKeyState x TurnRight Up

  --Switch input function depending on current mode
  input :: Event -> GameState -> IO GameState
  input e gs@GameState{mode = m} = execQueuedIO =<< 
    (\tgs -> (case m of
    Menu -> eventMenu e tgs
    Playing -> eventPlaying e tgs
    Paused -> eventPaused e tgs
    Score -> eventScore e tgs
    Settings -> eventSettings e tgs)) =<< updateKeyState e gs

  --Update keystate in GameState
  updateKeyState :: Event -> GameState -> IO GameState
  updateKeyState e gs@GameState { inputState = is }= case inputmode is of
    Keyboard -> updateByKeyboard e gs
    Mouse -> updateByMouse e gs
  
  --Update keyboard using events
  updateByKeyboard :: Event -> GameState -> IO GameState
  updateByKeyboard (EventMotion (x, y)) gs = return (alterMouseState gs (x, y))
  updateByKeyboard (EventKey (SpecialKey k) s _ _) gs = case k of
    KeyUp -> alterKeyState gs Forward s
    KeyDown -> alterKeyState gs Backward s
    KeyLeft -> alterKeyState gs TurnLeft s
    KeyRight -> alterKeyState gs TurnRight s 
    KeySpace -> alterKeyState gs Shoot s 
    KeyEnter -> alterKeyState gs Start s 
    KeyEsc -> alterKeyState gs Pause s 
    _ -> return gs
  updateByKeyboard _ gs = return gs

  --Update mouse using events
  updateByMouse :: Event -> GameState -> IO GameState
  updateByMouse (EventMotion (x, y)) gs = return (alterMouseState gs (x, y))
  updateByMouse (EventKey (MouseButton k) s _ _) gs = case k of
    LeftButton -> alterKeyState gs Shoot s
    RightButton -> alterKeyState gs Forward s 
    _ -> return gs
  updateByMouse (EventKey (SpecialKey k) s _ _) gs = case k of
    KeyDown -> alterKeyState gs Backward s
    KeyEnter -> alterKeyState gs Start s
    KeyEsc -> alterKeyState gs Pause s 
    _ -> return gs
  updateByMouse _ gs = return gs

  --Alter keystate inside gamestate
  alterKeyState :: GameState -> GameKey -> KeyState -> IO GameState
  alterKeyState gs@GameState{inputState = inState} k s = return gs{inputState = inState { keys = newKeys }}
    where 
      newKeys = (\(GameKeyState kx sx) -> case () of 
        _ | k == kx -> GameKeyState k s
          | otherwise -> GameKeyState kx sx) <$> keys inState

  --Alter mousestate inside gamestate
  alterMouseState :: GameState -> (Float, Float) -> GameState
  alterMouseState gs l = gs { inputState = (inputState gs) {
    mouse = l
  }}

  --Execute IO commands and reset processIO
  execQueuedIO :: GameState -> IO GameState
  execQueuedIO gs@GameState { processIO = fn } = fn gs { processIO = return }
