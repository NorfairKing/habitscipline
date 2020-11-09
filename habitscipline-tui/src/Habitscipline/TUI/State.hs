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
        newHabitStateUnit :: TextCursor,
        newHabitStateGoalType :: HabitType,
        newHabitStateGoalBoolean :: Bool,
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
      habitUnit = rebuildTextCursor newHabitStateUnit
  let parseWord d tc =
        let t = rebuildTextCursor tc
         in if T.null t
              then pure d
              else case readMaybe $ T.unpack t of
                Nothing -> Left $ "Not a number: " <> t
                Just w -> pure w
  let goalType = newHabitStateGoalType
  let goalBoolean = newHabitStateGoalBoolean
  goalNumerator <- parseWord 1 newHabitStateGoalNumerator
  goalDenominator <- parseWord 1 newHabitStateGoalDenominator
  let habitGoal = Goal {..}
  pure Habit {..}

data NewHabitStateSelection
  = SelectName
  | SelectDescription
  | SelectUnit
  | SelectGoalType
  | SelectGoalBoolean
  | SelectGoalNumerator
  | SelectGoalDenominator
  | SelectCancelButton
  | SelectCreateButton
  deriving (Show, Eq, Ord)

data ResourceName = ResourceTextCursor
  deriving (Show, Eq, Ord)

data Load a = Loaded a | Loading
  deriving (Show, Eq)

instance Functor Load where
  fmap _ Loading = Loading
  fmap f (Loaded a) = Loaded $ f a

instance Applicative Load where
  pure = Loaded
  Loaded f <*> Loaded a = Loaded $ f a
  _ <*> _ = Loading
