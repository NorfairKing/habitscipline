{-# LANGUAGE DeriveGeneric #-}
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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Habitscipline.API.Server.Data.DB where

import Data.Mergeful
import Data.Mergeful.Persistent ()
import Data.Password
import Data.Password.Instances ()
import Data.Text (Text)
import Data.Time
import Data.Validity
import Data.Validity.Persist ()
import Data.Validity.Time ()
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)
import Habitscipline.API.Server.Data.Username
import Habitscipline.Data

share
  [mkPersist sqlSettings, mkMigrate "serverMigration"]
  [persistLowerCase|

User
  name Username
  password PassHash

  UniqueUsername name

  deriving Show Eq Ord Generic


ServerHabit sql=habit
  user UserId

  serverTime ServerTime

  name Text
  description Text Maybe
  type HabitType
  goalUnit Text
  goalNumerator Word
  goalDenominator Word

  deriving Show Eq Ord Generic


ServerEntry sql=entry
  user UserId

  serverTime ServerTime

  day Day -- Not modifyable
  amount Word

  UniqueEntryDay user day
|]

instance Validity Salt where
  validate = trivialValidation

instance Validity Pass where
  validate = trivialValidation

instance Validity PassHash where
  validate = trivialValidation

instance Validity User

instance Validity ServerHabit

serverMakeHabit :: ServerHabit -> Timed Habit
serverMakeHabit ServerHabit {..} = Timed Habit {..} serverHabitServerTime
  where
    habitName = serverHabitName
    habitDescription = serverHabitDescription
    habitType = serverHabitType
    goalUnit = serverHabitGoalUnit
    goalNumerator = serverHabitGoalNumerator
    goalDenominator = serverHabitGoalDenominator
    habitGoal = Goal {..}

makeServerHabit :: UserId -> Habit -> ServerHabit
makeServerHabit serverHabitUser Habit {..} = ServerHabit {..}
  where
    serverHabitServerTime = initialServerTime
    serverHabitName = habitName
    serverHabitDescription = habitDescription
    serverHabitType = habitType
    Goal {..} = habitGoal
    serverHabitGoalUnit = goalUnit
    serverHabitGoalNumerator = goalNumerator
    serverHabitGoalDenominator = goalDenominator

serverMakeEntry :: ServerEntry -> Timed Entry
serverMakeEntry ServerEntry {..} = Timed Entry {..} serverEntryServerTime
  where
    entryAmount = serverEntryAmount
    entryDay = serverEntryDay

makeServerEntry :: UserId -> Entry -> ServerEntry
makeServerEntry serverEntryUser Entry {..} = ServerEntry {..}
  where
    serverEntryServerTime = initialServerTime
    serverEntryAmount = entryAmount
    serverEntryDay = entryDay
