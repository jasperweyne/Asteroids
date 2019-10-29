-- | This module defines how the state changes
--   in response to time and user input
module Controller (step, input) where

  import Model

  import Graphics.Gloss
  import Graphics.Gloss.Interface.IO.Game
  import Type.State
  import Controller.Menu
  import Controller.Playing

  -- | Handle one iteration of the game
  step :: Float -> GameState -> IO GameState
  step t gs@GameState{mode = m} = case m of
    Menu -> stepMenu t gs
    Playing -> stepPlaying t gs
    _ -> stepMenu t gs

  -- | Handle user input
  input :: Event -> GameState -> IO GameState
  input e gs@GameState{mode = m} = case m of
    Menu -> eventMenu e gs
    Playing -> eventPlaying e gs
    _ -> eventMenu e gs