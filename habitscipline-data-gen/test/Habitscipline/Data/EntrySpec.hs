{-# LANGUAGE TypeApplications #-}

module Habitscipline.Data.EntrySpec
  ( spec,
  )
where

import qualified Data.Map as M
import Habitscipline.Data.Entry
import Habitscipline.Data.Entry.Gen ()
import Habitscipline.Data.Habit.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Entry
  jsonSpec @Entry
  genValidSpec @EntryMap
  jsonSpec @EntryMap
  describe "entryMapRangeSum" $
    it "produces valid amounts" $
      forAllValid $ \b -> forAllValid $ \em -> forAllValid $ \begin -> forAllValid $ \end ->
        shouldBeValid $ entryMapRangeSum b em begin end
  describe "entryMapGoalMet" $
    it "produces valid results" $
      forAllValid $ \g -> forAllValid $ \d -> forAllValid $ \em ->
        shouldBeValid $ entryMapGoalMet g d em
  describe "entryMapStreaks" $
    it "produces valid streaks" $
      forAllValid $ \g -> forAllValid $ \em -> forAllValid $ \d ->
        shouldBeValid $ entryMapStreaks g em $ maybe d fst $ M.lookupMax (unEntryMap em)
  describe "entryMapLongestStreak" $
    it "produces valid streaks" $
      forAllValid $ \g -> forAllValid $ \em -> forAllValid $ \d ->
        shouldBeValid $ entryMapLongestStreak g em $ maybe d fst $ M.lookupMax (unEntryMap em)
  describe "entryMapLatestStreak" $
    it "produces valid streaks" $
      forAllValid $ \g -> forAllValid $ \em -> forAllValid $ \d ->
        shouldBeValid $ entryMapLatestStreak g em $ maybe d fst $ M.lookupMax (unEntryMap em)
