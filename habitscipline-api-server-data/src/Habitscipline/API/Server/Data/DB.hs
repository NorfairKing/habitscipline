{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Data.Password.Bcrypt
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

type PassHash = PasswordHash Bcrypt

share
  [mkPersist sqlSettings, mkMigrate "serverMigration"]
  [persistLowerCase|

User
  name Username
  password PassHash

  UniqueUsername name

  deriving Show Eq Ord Generic


ServerHabit sql=habit
  user UserId sql=user

  serverTime ServerTime sql=server_time

  uuid HabitUuid sql=uuid -- Not modifyable
  name Text sql=name
  description Text Maybe sql=description
  unit Text sql=unit
  goalType HabitType sql=type
  goalBoolean Bool sql=boolean
  goalNumerator Word sql=numerator
  goalDenominator Word sql=denominator

  deriving Show Eq Ord Generic


ServerEntry sql=entry
  user UserId sql=user

  serverTime ServerTime sql=server_time

  habit HabitUuid sql=uuid -- Not modifyable
  day Day sql=day -- Not modifyable
  amount Word sql=amount

  UniqueEntryDay habit user day
|]

instance Validity (Salt Bcrypt) where
  validate = trivialValidation

instance Validity Password where
  validate = trivialValidation

instance Validity PassHash where
  validate = trivialValidation

instance Validity User

instance Validity ServerHabit

serverMakeHabit :: ServerHabit -> Timed Habit
serverMakeHabit ServerHabit {..} = Timed Habit {..} serverHabitServerTime
  where
    habitUuid = serverHabitUuid
    habitName = serverHabitName
    habitDescription = serverHabitDescription
    habitUnit = serverHabitUnit
    goalType = serverHabitGoalType
    goalBoolean = serverHabitGoalBoolean
    goalNumerator = serverHabitGoalNumerator
    goalDenominator = serverHabitGoalDenominator
    habitGoal = Goal {..}

makeServerHabit :: UserId -> Habit -> ServerHabit
makeServerHabit serverHabitUser Habit {..} = ServerHabit {..}
  where
    serverHabitUuid = habitUuid
    serverHabitServerTime = initialServerTime
    serverHabitName = habitName
    serverHabitDescription = habitDescription
    serverHabitUnit = habitUnit
    Goal {..} = habitGoal
    serverHabitGoalType = goalType
    serverHabitGoalBoolean = goalBoolean
    serverHabitGoalNumerator = goalNumerator
    serverHabitGoalDenominator = goalDenominator

serverMakeEntry :: ServerEntry -> Timed Entry
serverMakeEntry ServerEntry {..} = Timed Entry {..} serverEntryServerTime
  where
    entryHabit = serverEntryHabit
    entryAmount = serverEntryAmount
    entryDay = serverEntryDay

makeServerEntry :: UserId -> Entry -> ServerEntry
makeServerEntry serverEntryUser Entry {..} = ServerEntry {..}
  where
    serverEntryServerTime = initialServerTime
    serverEntryHabit = entryHabit
    serverEntryAmount = entryAmount
    serverEntryDay = entryDay
