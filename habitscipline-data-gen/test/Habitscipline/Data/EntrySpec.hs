{-# LANGUAGE TypeApplications #-}

module Habitscipline.Data.EntrySpec
  ( spec,
  )
where

import Habitscipline.Data.Entry
import Habitscipline.Data.Entry.Gen ()
import Habitscipline.Data.Habit.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Entry
  jsonSpecOnValid @Entry
  genValidSpec @EntryMap
  jsonSpecOnValid @EntryMap
  describe "entryMapRangeSum" $ it "produces valid amounts" $ forAllValid $ \b -> forAllValid $ \em -> forAllValid $ \begin -> forAllValid $ \end ->
    shouldBeValid $ entryMapRangeSum b em begin end
  describe "entryMapGoalMet" $ it "produces valid results" $ forAllValid $ \ht -> forAllValid $ \b -> forAllValid $ \g -> forAllValid $ \d -> forAllValid $ \em ->
    shouldBeValid $ entryMapGoalMet ht b g d em
  describe "entryMapStreaks" $ it "produces valid streaks" $ forAllValid $ \ht -> forAllValid $ \b -> forAllValid $ \g -> forAllValid $ \em ->
    shouldBeValid $ entryMapStreaks ht b g em
  describe "entryMapLongestStreak" $ it "produces valid streaks" $ forAllValid $ \ht -> forAllValid $ \b -> forAllValid $ \g -> forAllValid $ \em ->
    shouldBeValid $ entryMapLongestStreak ht b g em
  describe "entryMapLatestStreak" $ it "produces valid streaks" $ forAllValid $ \ht -> forAllValid $ \b -> forAllValid $ \g -> forAllValid $ \em ->
    shouldBeValid $ entryMapLatestStreak ht b g em
