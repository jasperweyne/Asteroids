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

  -- | Handle one iteration of the game
  step :: Float -> GameState -> IO GameState
  step t gs@GameState{mode = m} = resetKeyState =<< execQueuedIO =<< 
    (\tgs -> (case m of
    Menu -> stepMenu t tgs
    Playing -> stepPlaying t tgs
    Paused -> stepPaused t tgs
    Score -> stepScore t tgs
    Settings -> stepSettings t tgs)) gs

  resetKeyState :: GameState -> IO GameState
  resetKeyState gs@GameState { inputState = is }= case inputmode is of
    Keyboard -> return gs
    Mouse -> do 
      x <- alterKeyState gs TurnLeft Up
      alterKeyState x TurnRight Up

  -- | Handle user input
  input :: Event -> GameState -> IO GameState
  input e gs@GameState{mode = m} = execQueuedIO =<< 
    (\tgs -> (case m of
    Menu -> eventMenu e tgs
    Playing -> eventPlaying e tgs
    Paused -> eventPaused e tgs
    Score -> eventScore e tgs
    Settings -> eventSettings e tgs)) =<< updateKeyState e gs

  updateKeyState :: Event -> GameState -> IO GameState
  updateKeyState e gs@GameState { inputState = is }= case inputmode is of
    Keyboard -> updateByKeyboard e gs
    Mouse -> updateByMouse e gs
  
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

  updateByMouse :: Event -> GameState -> IO GameState
  updateByMouse (EventMotion (x, y)) gs | mx - x >   5  = alterKeyState (alterMouseState gs (x, y)) TurnLeft Down
                                        | mx - x < (-5) = alterKeyState (alterMouseState gs (x, y)) TurnRight Down
                                        | otherwise     = return (alterMouseState gs (x, y))
    where mx = fst . mouse . inputState $ gs
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

  alterKeyState :: GameState -> GameKey -> KeyState -> IO GameState
  alterKeyState gs@GameState{inputState = inState} k s = return gs{inputState = inState { keys = newKeys }}
    where 
      newKeys = (\(GameKeyState kx sx) -> case () of 
        _ | k == kx -> GameKeyState k s
          | otherwise -> GameKeyState kx sx) <$> keys inState

  alterMouseState :: GameState -> (Float, Float) -> GameState
  alterMouseState gs l = gs { inputState = (inputState gs) {
    mouse = l
  }}

  execQueuedIO :: GameState -> IO GameState
  execQueuedIO gs@GameState { processIO = fn } = fn gs { processIO = return }
