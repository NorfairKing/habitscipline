{-# LANGUAGE TypeApplications #-}

module Habitscipline.Data.HabitSpec
  ( spec,
  )
where

import Habitscipline.Data.Habit
import Habitscipline.Data.Habit.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Persist

spec :: Spec
spec = do
  genValidSpec @Habit
  jsonSpec @Habit
  genValidSpec @HabitType
  jsonSpec @HabitType
  persistSpec @HabitType
  genValidSpec @Goal
  jsonSpec @Goal
