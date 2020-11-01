{-# LANGUAGE RecordWildCards #-}

module Habitscipline.API.Server.Handler.Sync where

import Data.Mergeful.Persistent
import Database.Persist
import Habitscipline.API.Server.Handler.Import

handlePostSync :: AuthCookie -> SyncRequest -> H SyncResponse
handlePostSync AuthCookie {..} SyncRequest {..} = withUser authCookieUsername $ \(Entity uid _) -> runDB $ do
  syncResponseHabitSyncResponse <-
    serverProcessSyncQuery
      ServerHabitServerTime
      [ServerHabitUser ==. uid]
      serverMakeHabit
      (const (makeServerHabit uid))
      ( \Habit {..} ->
          let Goal {..} = habitGoal
           in [ ServerHabitName =. habitName,
                ServerHabitDescription =. habitDescription,
                ServerHabitType =. habitType,
                ServerHabitGoalUnit =. goalUnit,
                ServerHabitGoalNumerator =. goalNumerator,
                ServerHabitGoalDenominator =. goalDenominator
              ]
      )
      syncRequestHabitSyncRequest
  syncResponseEntrySyncResponse <-
    serverProcessSyncQuery
      ServerEntryServerTime
      [ServerEntryUser ==. uid]
      serverMakeEntry
      (const (makeServerEntry uid))
      ( \Entry {..} ->
          [ ServerEntryAmount =. entryAmount
          ]
      )
      syncRequestEntrySyncRequest
  pure SyncResponse {..}
