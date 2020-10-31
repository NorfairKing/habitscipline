{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Habitscipline.Data.Habit where

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)

data Habit
  = Habit
      { habitName :: !Text,
        habitDescription :: !(Maybe Text),
        habitType :: !HabitType,
        habitGoal :: !Goal
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity Habit

instance FromJSON Habit where
  parseJSON = withObject "Habit" $ \o ->
    Habit
      <$> o .: "name"
      <*> o .:? "description"
      <*> o .: "type"
      <*> o .: "goal"

instance ToJSON Habit where
  toJSON Habit {..} =
    object
      [ "name" .= habitName,
        "description" .= habitDescription,
        "type" .= habitType,
        "goal" .= habitGoal
      ]

data HabitType = PositiveHabit | NegativeHabit
  deriving (Show, Eq, Ord, Generic)

instance Validity HabitType

instance FromJSON HabitType

instance ToJSON HabitType

renderHabitType :: HabitType -> Text
renderHabitType = \case
  PositiveHabit -> "Positive"
  NegativeHabit -> "Negative"

parseHabitType :: Text -> Maybe HabitType
parseHabitType = \case
  "Positive" -> Just PositiveHabit
  "Negative" -> Just NegativeHabit
  _ -> Nothing

instance PersistField HabitType where
  toPersistValue = toPersistValue . renderHabitType
  fromPersistValue pv = do
    t <- fromPersistValue pv
    case parseHabitType t of
      Nothing -> Left $ "Unknown habit type: " <> t
      Just ht -> pure ht

instance PersistFieldSql HabitType where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)

data Goal
  = Goal
      { goalUnit :: !Text, -- Trainings, Grams of sugar, ...
        goalNumerator :: !Word, -- How many of the unit
        goalDenominator :: !Word -- How many days
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity Goal

instance FromJSON Goal

instance ToJSON Goal
