{-# LANGUAGE TypeApplications #-}

module Habitscipline.Data.HabitSpec
  ( spec,
  )
where

import Habitscipline.Data.Habit
import Habitscipline.Data.Habit.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson
import Test.Validity.Persist

spec :: Spec
spec = do
  genValidSpec @Habit
  jsonSpec @Habit
  genValidSpec @HabitType
  jsonSpec @HabitType
  persistSpec @HabitType
  genValidSpec @Goal
  jsonSpec @Goal
