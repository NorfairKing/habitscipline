{-# OPTIONS_GHC -fno-warn-orphans #-}

module Habitscipline.Data.Habit.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Habitscipline.Data.Habit

instance GenValid Habit where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid HabitType where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Goal where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
