{-# LANGUAGE RecordWildCards #-}

module Habitscipline.CLI.Commands.Sync where

import Data.Maybe
import Data.Mergeful as Mergeful hiding (SyncRequest, SyncResponse)
import Data.Mergeful.Persistent
import Database.Persist
import Habitscipline.CLI.Commands.Import
import Habitscipline.Data

sync :: C ()
sync = withClient $ \cenv -> withLogin cenv $ \token -> do
  syncRequestHabitSyncRequest <-
    runDB $
      clientMakeSyncRequestQuery
        ClientHabitServerId
        ClientHabitServerTime
        ClientHabitChangedLocally
        ClientHabitDeletedLocally
        ((\(_, _, h) -> h) . clientMakeHabit)
        ((\(msid, mst, h) -> (fromJust msid, Timed h (fromJust mst))) . clientMakeHabit)
        ((\(msid, mst, _) -> (fromJust msid, fromJust mst)) . clientMakeHabit)
  syncRequestEntrySyncRequest <-
    runDB $
      clientMakeSyncRequestQuery
        ClientEntryServerId
        ClientEntryServerTime
        ClientEntryChangedLocally
        ClientEntryDeletedLocally
        ((\(_, _, h) -> h) . clientMakeEntry)
        ((\(msid, mst, h) -> (fromJust msid, Timed h (fromJust mst))) . clientMakeEntry)
        ((\(msid, mst, _) -> (fromJust msid, fromJust mst)) . clientMakeEntry)
  let req = SyncRequest {..}
  SyncResponse {..} <- runClientOrDie cenv $ postSync habitsciplineClient token req
  runDB $ do
    clientMergeSyncResponseQuery
      ClientHabitServerId
      ClientHabitServerTime
      ClientHabitChangedLocally
      ClientHabitDeletedLocally
      makeSyncedClientHabit
      ((\(msid, mst, h) -> (fromJust msid, Timed h (fromJust mst))) . clientMakeHabit)
      ( \Habit {..} ->
          let Goal {..} = habitGoal
           in [ ClientHabitName =. habitName,
                ClientHabitDescription =. habitDescription,
                ClientHabitType =. habitType,
                ClientHabitGoalUnit =. goalUnit,
                ClientHabitGoalNumerator =. goalNumerator,
                ClientHabitGoalDenominator =. goalDenominator
              ]
      )
      mergeFromServerStrategy
      syncResponseHabitSyncResponse
    clientMergeSyncResponseQuery
      ClientEntryServerId
      ClientEntryServerTime
      ClientEntryChangedLocally
      ClientEntryDeletedLocally
      makeSyncedClientEntry
      ((\(msid, mst, h) -> (fromJust msid, Timed h (fromJust mst))) . clientMakeEntry)
      ( \Entry {..} ->
          [ ClientEntryAmount =. entryAmount
          ]
      )
      mergeFromServerStrategy
      syncResponseEntrySyncResponse
