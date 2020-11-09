{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Habitscipline.Data.Habit.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.UUID.Typed ()
import Habitscipline.Data.Habit
import Test.QuickCheck

instance GenValid Habit where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid HabitType where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Goal where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = sized $ \s -> do
    goalBoolean <- genValid
    goalNumerator <- fromIntegral <$> choose (0, s) -- Really big numerators and denominators don't help
    goalDenominator <- max 1 . fromIntegral <$> choose (1, s)
    pure Goal {..}
