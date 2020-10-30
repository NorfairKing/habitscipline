module Habitscipline.TUI.State where

import Habitscipline.Data

data State = State {stateHabits :: Load [Habit]}
  deriving (Show)

data ResourceName = ResourceName
  deriving (Show, Eq, Ord)

data Load a = Loaded a | Loading
  deriving (Show, Eq)
