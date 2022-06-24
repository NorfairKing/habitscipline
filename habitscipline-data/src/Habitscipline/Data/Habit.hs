{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Habitscipline.Data.Habit where

import Autodocodec
import Control.Arrow (left)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
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

data Habit = Habit
  { habitUuid :: !HabitUuid,
    habitName :: !Text,
    habitDescription :: !(Maybe Text),
    habitUnit :: !Text, -- Trainings, Grams of sugar, ...
    habitGoal :: !Goal
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Habit)

instance Validity Habit

instance HasCodec Habit where
  codec =
    object "Habit" $
      Habit
        <$> requiredField "uuid" "habit uuid" .= habitUuid
        <*> requiredField "name" "habit name" .= habitName
        <*> optionalField "description" "habit description" .= habitDescription
        <*> requiredField "unit" "unit of the habit entries" .= habitUnit
        <*> requiredField "goal" "habit goal" .= habitGoal

data HabitType = PositiveHabit | NegativeHabit
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec HabitType)

instance Validity HabitType

instance HasCodec HabitType where
  codec = bimapCodec parseHabitType renderHabitType codec

renderHabitType :: HabitType -> Text
renderHabitType = \case
  PositiveHabit -> "Positive"
  NegativeHabit -> "Negative"

parseHabitType :: Text -> Either String HabitType
parseHabitType = \case
  "Positive" -> Right PositiveHabit
  "Negative" -> Right NegativeHabit
  _ -> Left "Unknown habit type"

instance PersistField HabitType where
  toPersistValue = toPersistValue . renderHabitType
  fromPersistValue pv = do
    t <- fromPersistValue pv
    left T.pack $ parseHabitType t

instance PersistFieldSql HabitType where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)

data Goal = Goal
  { goalType :: !HabitType, -- Positive or negative
    goalBoolean :: !Bool, -- Whether it's a boolean habit
    goalNumerator :: !Word, -- How many of the unit
    goalDenominator :: !Word -- How many days
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Goal)

instance Validity Goal where
  validate g@Goal {..} =
    mconcat
      [ genericValidate g,
        declare "The denominator is not zero" $ goalDenominator > 0
      ]

instance HasCodec Goal where
  codec =
    object "Goal" $
      Goal
        <$> requiredField "type" "habit type" .= goalType
        <*> requiredField "boolean" "whether the habit is boolean" .= goalBoolean
        <*> requiredField "numerator" "How many of the unit" .= goalNumerator
        <*> requiredField "denominator" "How many days" .= goalDenominator
