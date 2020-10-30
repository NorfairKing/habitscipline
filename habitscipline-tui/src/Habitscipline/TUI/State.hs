module Habitscipline.TUI.State where

import Cursor.Text
import Habitscipline.Data

data State
  = StateHabitList HabitListState
  | StateNewHabit NewHabitState
  deriving (Show)

data HabitListState
  = HabitListState
      { habitListStateHabits :: Load [Habit]
      }
  deriving (Show)

data NewHabitState
  = NewHabitState
      { newHabitStateName :: TextCursor,
        newHabitStateDescription :: TextCursor,
        newHabitStateType :: HabitType,
        newHabitStateGoalUnit :: TextCursor,
        newHabitStateGoalNumerator :: TextCursor,
        newHabitStateGoalDenominator :: TextCursor,
        newHabitStateSelection :: NewHabitStateSelection
      }
  deriving (Show)

data NewHabitStateSelection
  = SelectName
  | SelectDescription
  | SelectType
  | SelectGoalUnit
  | SelectGoalNumerator
  | SelectGoalDenominator
  deriving (Show, Eq, Ord)

data ResourceName = ResourceTextCursor
  deriving (Show, Eq, Ord)

data Load a = Loaded a | Loading
  deriving (Show, Eq)
