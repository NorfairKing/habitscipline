{-# LANGUAGE RecordWildCards #-}

module Habitscipline.TUI.Worker where

import Brick.BChan
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as M
import Database.Persist
import Habitscipline.Client.Data
import Habitscipline.Data
import Habitscipline.TUI.Env

tuiWorker :: BChan Request -> BChan Response -> W ()
tuiWorker reqChan respChan = forever $ do
  req <- liftIO $ readBChan reqChan
  mResp <- case req of
    RequestHistory -> do
      habits <- runDB $ map (clientMakeHabit_ . entityVal) <$> selectList [] []
      entries <- runDB $ map (clientMakeEntry_ . entityVal) <$> selectList [] []
      let maps = M.fromList $ flip map habits $ \habit ->
            let relevantEntries = filter ((== habitUuid habit) . entryHabit) entries
             in (habit, EntryMap $ M.fromList $ map (\Entry {..} -> (entryDay, entryAmount)) relevantEntries)
      pure $ Just $ ResponseHistory maps
    RequestHabits -> do
      habits <- runDB $ map (clientMakeHabit_ . entityVal) <$> selectList [] []
      pure $ Just $ ResponseHabits habits
    RequestCreateHabit h -> do
      runDB $ insert_ $ makeUnsyncedClientHabit h
      pure Nothing
  forM mResp $ \resp -> liftIO $ writeBChan respChan resp
