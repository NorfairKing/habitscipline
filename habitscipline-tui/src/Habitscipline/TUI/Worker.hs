module Habitscipline.TUI.Worker where

import Brick.BChan
import Control.Monad
import Control.Monad.IO.Class
import Database.Persist
import Habitscipline.Client.Data
import Habitscipline.TUI.Env

tuiWorker :: BChan Request -> BChan Response -> W ()
tuiWorker reqChan respChan = forever $ do
  req <- liftIO $ readBChan reqChan
  resp <- case req of
    RequestHabits -> do
      habits <- runDB $ map (clientMakeHabit_ . entityVal) <$> selectList [] []
      pure $ ResponseHabits habits
  liftIO $ writeBChan respChan resp
