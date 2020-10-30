{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Habitscipline.TUI where

import Brick.BChan
import Brick.Main
import Control.Concurrent.Async
import Control.Monad.Reader
import Graphics.Vty (defaultConfig, mkVty)
import Habitscipline.TUI.Draw
import Habitscipline.TUI.Env
import Habitscipline.TUI.Handle
import Habitscipline.TUI.OptParse
import Habitscipline.TUI.State
import Habitscipline.TUI.Worker

habitsciplineTUI :: IO ()
habitsciplineTUI = do
  Settings {..} <- getSettings
  initialState <- buildInitialState
  reqChan <- newBChan 1000
  respChan <- newBChan 1000
  let vtyBuilder = mkVty defaultConfig
  firstVty <- vtyBuilder
  let runTui = customMain firstVty vtyBuilder (Just respChan) (tuiApp reqChan) initialState
  let env = Env
  let runWorker = runReaderT (tuiWorker reqChan respChan) env
  -- Left always works because the worker runs forever
  Left endState <- race runTui runWorker
  print endState

tuiApp :: BChan Request -> App State Response ResourceName
tuiApp chan =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent chan,
      appStartEvent = pure,
      appAttrMap = buildAttrMap
    }

buildInitialState :: IO State
buildInitialState = pure State
