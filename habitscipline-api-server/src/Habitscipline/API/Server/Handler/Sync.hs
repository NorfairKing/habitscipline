{-# LANGUAGE RecordWildCards #-}

module Habitscipline.API.Server.Handler.Sync where

import Data.Mergeful.Persistent
import Database.Persist
import Habitscipline.API.Server.Handler.Import

handlePostSync :: AuthCookie -> SyncRequest -> H SyncResponse
handlePostSync AuthCookie {..} sr = withUser authCookieUsername $ \(Entity uid _) ->
  runDB $
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
      sr
