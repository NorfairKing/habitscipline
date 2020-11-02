{-# LANGUAGE RecordWildCards #-}

module Habitscipline.TUI.Worker where

import Brick.BChan
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Map (Map)
import Database.Persist
import Database.Persist.Sql
import Habitscipline.Client.Data
import Habitscipline.Data
import Habitscipline.TUI.Env

tuiWorker :: BChan Request -> BChan Response -> W ()
tuiWorker reqChan respChan = forever $ do
  req <- liftIO $ readBChan reqChan
  mResp <- case req of
    RequestHistory -> do
      maps <- runDB calculateHistory
      pure $ Just $ ResponseHistory maps
    RequestHabits -> do
      habits <- runDB $ map (clientMakeHabit_ . entityVal) <$> selectList [] []
      pure $ Just $ ResponseHabits habits
    RequestSetEntry Entry {..} -> do
      _ <-
        runDB $
          upsertBy
            (UniqueClientEntryDay entryHabit entryDay)
            ( ClientEntry
                { clientEntryServerId = Nothing,
                  clientEntryServerTime = Nothing,
                  clientEntryDeletedLocally = False,
                  clientEntryChangedLocally = False,
                  clientEntryHabit = entryHabit,
                  clientEntryDay = entryDay,
                  clientEntryAmount = entryAmount
                }
            )
            [ ClientEntryChangedLocally =. True,
              ClientEntryAmount =. entryAmount
            ]
      maps <- runDB calculateHistory
      pure $ Just $ ResponseHistory maps
    RequestCreateHabit h -> do
      runDB $ insert_ $ makeUnsyncedClientHabit h
      pure Nothing
  forM mResp $ \resp -> liftIO $ writeBChan respChan resp

calculateHistory :: SqlPersistT IO (Map Habit EntryMap)
calculateHistory = do
  habits <- map (clientMakeHabit_ . entityVal) <$> selectList [] []
  entries <- map (clientMakeEntry_ . entityVal) <$> selectList [] []
  let maps = M.fromList $ flip map habits $ \habit ->
        let relevantEntries = filter ((== habitUuid habit) . entryHabit) entries
         in (habit, EntryMap $ M.fromList $ map (\Entry {..} -> (entryDay, entryAmount)) relevantEntries)
  pure maps
