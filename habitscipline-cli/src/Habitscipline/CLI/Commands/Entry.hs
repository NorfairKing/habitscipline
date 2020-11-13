{-# LANGUAGE RecordWildCards #-}

module Habitscipline.CLI.Commands.Entry where

import Data.UUID.Typed
import Database.Persist
import Habitscipline.CLI.Commands.Import

entry :: EntrySettings -> C ()
entry EntrySettings {..} =
  runDB $ do
    mHabitByUuid <- fmap join $ forM (parseUUIDText entrySettingHabit) $ \uuid -> getBy $ UniqueClientHabitUuid uuid
    habits <- case mHabitByUuid of
      Just ch -> pure [ch]
      Nothing -> selectList [ClientHabitName ==. entrySettingHabit] []
    forM_ habits $ \(Entity _ habit) -> do
      let uuid = clientHabitUuid habit
          newEntry =
            ClientEntry
              { clientEntryServerId = Nothing,
                clientEntryServerTime = Nothing,
                clientEntryDeletedLocally = False,
                clientEntryChangedLocally = False,
                clientEntryHabit = uuid,
                clientEntryDay = entrySettingDay,
                clientEntryAmount = entrySettingAmount
              }
          updates =
            [ ClientEntryAmount =. entrySettingAmount,
              ClientEntryChangedLocally =. True
            ]
      upsertBy (UniqueClientEntryDay uuid entrySettingDay) newEntry updates
