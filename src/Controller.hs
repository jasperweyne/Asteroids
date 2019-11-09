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

  -- | Handle one iteration of the game
  step :: Float -> GameState -> IO GameState
  step t gs@GameState{mode = m} = execQueuedIO =<< case m of
    Menu -> stepMenu t gs
    Playing -> stepPlaying t gs
    Paused -> stepPaused t gs
    Score -> stepScore t gs

  -- | Handle user input
  input :: Event -> GameState -> IO GameState
  input e gs@GameState{mode = m} = execQueuedIO =<< 
    (\tgs -> (case m of
    Menu -> eventMenu e tgs
    Playing -> eventPlaying e tgs
    Paused -> eventPaused e tgs
    Score -> eventScore e tgs)) =<< updateKeyState e gs

  updateKeyState :: Event -> GameState -> IO GameState
  updateKeyState (EventKey (SpecialKey k) s _ _) gs = case k of
    KeyUp -> alterKeyState gs Forward s
    KeyDown -> alterKeyState gs Backward s
    KeyLeft -> alterKeyState gs TurnLeft s
    KeyRight -> alterKeyState gs TurnRight s 
    KeySpace -> alterKeyState gs Shoot s 
    KeyEnter -> alterKeyState gs Start s 
    KeyEsc -> alterKeyState gs Pause s 
    _ -> return gs
  updateKeyState _ gs = return gs

  alterKeyState :: GameState -> GameKey -> KeyState -> IO GameState
  alterKeyState gs@GameState{inputState = inState} k s = return gs{inputState = InputState 0 newKeys (screen inState)}
    where 
      newKeys = (\(GameKeyState kx sx) -> case () of 
        _ | k == kx -> GameKeyState k s
          | otherwise -> GameKeyState kx sx) <$> keys inState

  execQueuedIO :: GameState -> IO GameState
  execQueuedIO gs@GameState { processIO = fn } = fn gs { processIO = return }
