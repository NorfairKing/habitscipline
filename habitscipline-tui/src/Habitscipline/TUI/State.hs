{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Habitscipline.TUI.State where

import Cursor.Simple.List.NonEmpty
import Cursor.Text
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Habitscipline.Data
import Text.Read

data State
  = StateHistory HistoryState
  | StateHabitList HabitListState
  | StateNewHabit NewHabitState
  deriving (Show)

data HistoryState
  = HistoryState
      { historyStateHabitMaps :: !(Load (Map Habit EntryMap)),
        historyStateHabitCursor :: !(Load (Maybe (NonEmptyCursor HabitUuid))),
        historyStateAmountCursor :: !TextCursor,
        historyStateToday :: !Day,
        historyStateDay :: !Day,
        historyStateMaxDay :: !Day
      }
  deriving (Show)

data HabitListState
  = HabitListState
      { habitListStateHabits :: !(Load (Maybe (NonEmptyCursor Habit)))
      }
  deriving (Show)

data NewHabitState
  = NewHabitState
      { newHabitStateName :: TextCursor,
        newHabitStateDescription :: TextCursor,
        newHabitStateType :: HabitType,
        newHabitStateBoolean :: Bool,
        newHabitStateGoalUnit :: TextCursor,
        newHabitStateGoalNumerator :: TextCursor,
        newHabitStateGoalDenominator :: TextCursor,
        newHabitStateSelection :: NewHabitStateSelection
      }
  deriving (Show)

newHabitStateCompleteHabit :: HabitUuid -> NewHabitState -> Either Text Habit
newHabitStateCompleteHabit habitUuid NewHabitState {..} = do
  let habitName = rebuildTextCursor newHabitStateName
      habitDescription =
        let t = rebuildTextCursor newHabitStateDescription
         in if T.null t then Nothing else Just t
      habitType = newHabitStateType
      habitBoolean = newHabitStateBoolean
  let parseWord d tc =
        let t = rebuildTextCursor tc
         in if T.null t
              then pure d
              else case readMaybe $ T.unpack t of
                Nothing -> Left $ "Not a number: " <> t
                Just w -> pure w
  let goalUnit = rebuildTextCursor newHabitStateGoalUnit
  goalNumerator <- parseWord 1 newHabitStateGoalNumerator
  goalDenominator <- parseWord 1 newHabitStateGoalDenominator
  let habitGoal = Goal {..}
  pure Habit {..}

data NewHabitStateSelection
  = SelectName
  | SelectDescription
  | SelectType
  | SelectGoalUnit
  | SelectGoalNumerator
  | SelectGoalDenominator
  | SelectCancelButton
  | SelectCreateButton
  deriving (Show, Eq, Ord)

data ResourceName = ResourceTextCursor
  deriving (Show, Eq, Ord)

data Load a = Loaded a | Loading
  deriving (Show, Eq)
