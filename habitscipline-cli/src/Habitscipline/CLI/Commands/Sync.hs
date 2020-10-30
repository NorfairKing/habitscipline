{-# LANGUAGE RecordWildCards #-}

module Habitscipline.CLI.Commands.Sync where

import Data.Maybe
import Data.Mergeful
import Data.Mergeful.Persistent
import Database.Persist
import Habitscipline.CLI.Commands.Import
import Habitscipline.Data

sync :: C ()
sync = withClient $ \cenv -> withLogin cenv $ \token -> do
  req <-
    runDB $
      clientMakeSyncRequestQuery
        ClientHabitServerId
        ClientHabitServerTime
        ClientHabitChangedLocally
        ClientHabitDeletedLocally
        ((\(_, _, h) -> h) . clientMakeHabit)
        ((\(msid, mst, h) -> (fromJust msid, Timed h (fromJust mst))) . clientMakeHabit)
        ((\(msid, mst, _) -> (fromJust msid, fromJust mst)) . clientMakeHabit)
  resp <- runClientOrDie cenv $ postSync habitsciplineClient token req
  runDB $
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
      resp
