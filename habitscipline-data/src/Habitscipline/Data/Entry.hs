{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Habitscipline.Data.Entry where

import Data.Aeson
import qualified Data.Map as M
import Data.Map
import Data.Time
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Habitscipline.Data.Habit

data Entry
  = Entry
      { entryHabit :: HabitUuid,
        entryDay :: !Day,
        entryAmount :: !Word
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity Entry

instance FromJSON Entry where
  parseJSON = withObject "Entry" $ \o ->
    Entry
      <$> o .: "habit"
      <*> o .: "day"
      <*> o .: "amount"

instance ToJSON Entry where
  toJSON Entry {..} =
    object
      [ "habit" .= entryHabit,
        "day" .= entryDay,
        "amount" .= entryAmount
      ]

newtype EntryMap
  = EntryMap
      { unEntryMap :: Map Day Word
      }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance Validity EntryMap

data DayAmount
  = AssumedZero
  | NoDataBeforeFirst
  | NoDataAfterLast
  | Exactly Word

entryMapLookup :: EntryMap -> Day -> DayAmount
entryMapLookup (EntryMap m) d = case M.lookup d m of
  Just w -> Exactly w
  Nothing -> case (M.lookupGE d m, M.lookupLE d m) of
    (Nothing, Nothing) -> NoDataBeforeFirst
    (Nothing, Just _) -> NoDataAfterLast
    (Just _, Nothing) -> NoDataBeforeFirst
    (Just _, Just _) -> AssumedZero
