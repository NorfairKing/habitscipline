{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Habitscipline.Client.Data.DB where

import Data.Mergeful
import Data.Mergeful.Persistent ()
import Data.Text (Text)
import Database.Persist.Sqlite
import Database.Persist.TH
import Habitscipline.API.Server.Data
import Habitscipline.Data

share
  [mkPersist sqlSettings, mkMigrate "clientMigration"]
  [persistLowerCase|

ClientHabit sql=habit
    serverId ServerHabitId Maybe
    serverTime ServerTime Maybe
    deletedLocally Bool
    changedLocally Bool

    name Text
    description Text Maybe
    type HabitType
    goalUnit Text
    goalNumerator Word
    goalDenominator Word

    deriving Show Eq
|]

clientMakeHabit :: ClientHabit -> (Maybe ServerHabitId, Maybe ServerTime, Habit)
clientMakeHabit ClientHabit {..} = (clientHabitServerId, clientHabitServerTime, Habit {..})
  where
    habitName = clientHabitName
    habitDescription = clientHabitDescription
    habitType = clientHabitType
    goalUnit = clientHabitGoalUnit
    goalNumerator = clientHabitGoalNumerator
    goalDenominator = clientHabitGoalDenominator
    habitGoal = Goal {..}

makeSyncedClientHabit :: ServerHabitId -> Timed Habit -> ClientHabit
makeSyncedClientHabit sid (Timed Habit {..} st) = ClientHabit {..}
  where
    clientHabitServerId = Just sid
    clientHabitDeletedLocally = False
    clientHabitChangedLocally = False
    clientHabitServerTime = Just st
    clientHabitName = habitName
    clientHabitDescription = habitDescription
    clientHabitType = habitType
    Goal {..} = habitGoal
    clientHabitGoalUnit = goalUnit
    clientHabitGoalNumerator = goalNumerator
    clientHabitGoalDenominator = goalDenominator
