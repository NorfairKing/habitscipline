{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Habitscipline.Data.Habit where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Text (Text)
import Data.UUID.Typed
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)

instance PersistField (UUID a) where
  toPersistValue uuid = toPersistValue $ uuidASCIIBytes uuid
  fromPersistValue pv = do
    bs <- fromPersistValue pv
    case parseUUIDAsciiBytes bs of
      Nothing -> Left "Invalidy Bytestring to convert to UUID"
      Just uuid -> pure uuid

instance PersistFieldSql (UUID a) where
  sqlType Proxy = sqlType (Proxy :: Proxy ByteString)

type HabitUuid = UUID Habit

data Habit
  = Habit
      { habitUuid :: !HabitUuid,
        habitName :: !Text,
        habitDescription :: !(Maybe Text),
        habitUnit :: !Text, -- Trainings, Grams of sugar, ...
        habitGoal :: !Goal
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity Habit

instance FromJSON Habit where
  parseJSON = withObject "Habit" $ \o ->
    Habit
      <$> o .: "uuid"
      <*> o .: "name"
      <*> o .:? "description"
      <*> o .: "unit"
      <*> o .: "goal"

instance ToJSON Habit where
  toJSON Habit {..} =
    object
      [ "uuid" .= habitUuid,
        "name" .= habitName,
        "description" .= habitDescription,
        "unit" .= habitUnit,
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
      { goalType :: !HabitType, -- Positive or negative
        goalBoolean :: !Bool, -- Whether it's a boolean habit
        goalNumerator :: !Word, -- How many of the unit
        goalDenominator :: !Word -- How many days
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity Goal where
  validate g@Goal {..} = mconcat [genericValidate g, declare "The denominator is not zero" $ goalDenominator > 0]

instance FromJSON Goal where
  parseJSON = withObject "Goal" $ \o ->
    Goal
      <$> o .: "type"
      <*> o .: "boolean"
      <*> o .: "numerator"
      <*> o .: "denominator"

instance ToJSON Goal where
  toJSON Goal {..} =
    object
      [ "type" .= goalType,
        "boolean" .= goalBoolean,
        "numerator" .= goalNumerator,
        "denominator" .= goalDenominator
      ]
