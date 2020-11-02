{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Habitscipline.TUI where

import Brick.BChan
import Brick.Main
import Control.Concurrent.Async
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Database.Persist.Sqlite
import Graphics.Vty (defaultConfig, mkVty)
import Habitscipline.Client.Data
import Habitscipline.TUI.Draw
import Habitscipline.TUI.Env
import Habitscipline.TUI.Handle
import Habitscipline.TUI.OptParse
import Habitscipline.TUI.State
import Habitscipline.TUI.Worker
import Path
import Path.IO
import System.Exit
import System.FileLock

habitsciplineTUI :: IO ()
habitsciplineTUI = do
  Settings {..} <- getSettings
  ensureDir $ parent settingDbFile
  let dbFile = fromAbsFile settingDbFile
  let lockFilePath = dbFile ++ ".lock"
  mLocked <- withTryFileLock lockFilePath Exclusive $ \_ ->
    runNoLoggingT
      $ withSqlitePool (T.pack dbFile) 1
      $ \pool -> do
        _ <- runSqlPool (runMigrationQuiet clientMigration) pool
        liftIO $ do
          initialState <- buildInitialState
          reqChan <- newBChan 1000
          respChan <- newBChan 1000
          let vtyBuilder = mkVty defaultConfig
          firstVty <- vtyBuilder
          let runTui = customMain firstVty vtyBuilder (Just respChan) (tuiApp reqChan) initialState
          let env = Env {envConnectionPool = pool}
          let runWorker = runReaderT (tuiWorker reqChan respChan) env
          -- Left always works because the worker runs forever
          Left endState <- race runTui runWorker
          print endState
  case mLocked of
    Just () -> pure () -- Everything went file
    Nothing -> die "Unable to lock habit database."

tuiApp :: BChan Request -> App State Response ResourceName
tuiApp chan =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent chan,
      appStartEvent = \s -> do
        liftIO $ writeBChan chan RequestHistory
        pure s,
      appAttrMap = buildAttrMap
    }

buildInitialState :: IO State
buildInitialState =
  pure $ StateHistory $
    HistoryState
      { historyStateHabitMaps = Loading
      }
