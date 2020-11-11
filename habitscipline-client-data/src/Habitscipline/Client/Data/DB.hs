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
import Data.Time
import Database.Persist.Sqlite
import Database.Persist.TH
import Habitscipline.API.Server.Data
import Habitscipline.Data

share
  [mkPersist sqlSettings, mkMigrate "clientMigration"]
  [persistLowerCase|

ClientHabit sql=habit
    serverId ServerHabitId Maybe sql=server_id
    serverTime ServerTime Maybe sql=server_time
    deletedLocally Bool sql=deleted_locally
    changedLocally Bool sql=changed_locally

    uuid HabitUuid sql=uuid -- Not modifyable
    name Text sql=name
    description Text Maybe sql=description
    unit Text sql=unit
    goalType HabitType sql=type
    goalBoolean Bool sql=boolean
    goalNumerator Word sql=numerator
    goalDenominator Word sql=denominator

    UniqueClientHabitUuid uuid

    deriving Show Eq


ClientEntry sql=entry
    serverId ServerEntryId Maybe sql=server_id
    serverTime ServerTime Maybe sql=server_time
    deletedLocally Bool sql=deleted_locally
    changedLocally Bool sql=changed_locally

    habit HabitUuid sql=habit -- Not modifyable
    day Day sql=day -- Not modifyable
    amount Word sql=amount -- Modifyable

    UniqueClientEntryDay habit day
|]

clientMakeHabit :: ClientHabit -> (Maybe ServerHabitId, Maybe ServerTime, Habit)
clientMakeHabit ClientHabit {..} = (clientHabitServerId, clientHabitServerTime, Habit {..})
  where
    habitUuid = clientHabitUuid
    habitName = clientHabitName
    habitDescription = clientHabitDescription
    habitUnit = clientHabitUnit
    goalType = clientHabitGoalType
    goalBoolean = clientHabitGoalBoolean
    goalNumerator = clientHabitGoalNumerator
    goalDenominator = clientHabitGoalDenominator
    habitGoal = Goal {..}

clientMakeHabit_ :: ClientHabit -> Habit
clientMakeHabit_ = (\(_, _, h) -> h) . clientMakeHabit

makeUnsyncedClientHabit :: Habit -> ClientHabit
makeUnsyncedClientHabit = makeClientHabit Nothing Nothing

makeSyncedClientHabit :: ServerHabitId -> Timed Habit -> ClientHabit
makeSyncedClientHabit sid (Timed h st) = makeClientHabit (Just sid) (Just st) h

makeClientHabit :: Maybe ServerHabitId -> Maybe ServerTime -> Habit -> ClientHabit
makeClientHabit clientHabitServerId clientHabitServerTime Habit {..} = ClientHabit {..}
  where
    clientHabitDeletedLocally = False
    clientHabitChangedLocally = False
    clientHabitUuid = habitUuid
    clientHabitName = habitName
    clientHabitDescription = habitDescription
    clientHabitUnit = habitUnit
    Goal {..} = habitGoal
    clientHabitGoalBoolean = goalBoolean
    clientHabitGoalType = goalType
    clientHabitGoalNumerator = goalNumerator
    clientHabitGoalDenominator = goalDenominator

clientMakeEntry :: ClientEntry -> (Maybe ServerEntryId, Maybe ServerTime, Entry)
clientMakeEntry ClientEntry {..} = (clientEntryServerId, clientEntryServerTime, Entry {..})
  where
    entryHabit = clientEntryHabit
    entryDay = clientEntryDay
    entryAmount = clientEntryAmount

clientMakeEntry_ :: ClientEntry -> Entry
clientMakeEntry_ = (\(_, _, h) -> h) . clientMakeEntry

makeUnsyncedClientEntry :: Entry -> ClientEntry
makeUnsyncedClientEntry = makeClientEntry Nothing Nothing

makeSyncedClientEntry :: ServerEntryId -> Timed Entry -> ClientEntry
makeSyncedClientEntry sid (Timed h st) = makeClientEntry (Just sid) (Just st) h

makeClientEntry :: Maybe ServerEntryId -> Maybe ServerTime -> Entry -> ClientEntry
makeClientEntry clientEntryServerId clientEntryServerTime Entry {..} = ClientEntry {..}
  where
    clientEntryDeletedLocally = False
    clientEntryChangedLocally = False
    clientEntryHabit = entryHabit
    clientEntryDay = entryDay
    clientEntryAmount = entryAmount
