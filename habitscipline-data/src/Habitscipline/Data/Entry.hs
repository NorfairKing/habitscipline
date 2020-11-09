{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Habitscipline.Data.Entry where

import Data.Aeson
import qualified Data.Map as M
import Data.Map (Map)
import Data.Ord
import Data.Time
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Habitscipline.Data.Habit
import Safe

data Entry
  = Entry
      { entryHabit :: HabitUuid,
        entryDay :: !Day,
        entryAmount :: !Word
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity Entry

instance FromJSON Entry where
  parseJSON = withObject "Entry" $ \o ->
    Entry
      <$> o .: "habit"
      <*> o .: "day"
      <*> o .: "amount"

instance ToJSON Entry where
  toJSON Entry {..} =
    object
      [ "habit" .= entryHabit,
        "day" .= entryDay,
        "amount" .= entryAmount
      ]

newtype EntryMap
  = EntryMap
      { unEntryMap :: Map Day Word
      }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance Validity EntryMap

data DayAmount
  = AssumedZero
  | NoDataBeforeFirst
  | NoDataAfterLast
  | Exactly Word

entryMapLookup :: EntryMap -> Day -> DayAmount
entryMapLookup (EntryMap m) d = case M.lookup d m of
  Just w -> Exactly w
  Nothing -> case (M.lookupGE d m, M.lookupLE d m) of
    (Nothing, Nothing) -> NoDataBeforeFirst
    (Nothing, Just _) -> NoDataAfterLast
    (Just _, Nothing) -> NoDataBeforeFirst
    (Just _, Just _) -> AssumedZero

-- | A Streak is a tuple of days, a begin and an end, both inclusive.
--
-- A streak is a range of days where the habit goal was met every day.
data Streak
  = Streak
      { streakBegin :: !Day,
        streakEnd :: !Day
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity Streak where
  validate s@Streak {..} =
    mconcat
      [ genericValidate s,
        declare "The end is on or after the begin" $ streakEnd >= streakBegin
      ]

streakDays :: Streak -> Word
streakDays Streak {..} =
  -- This 'fromIntegral' is technically not correct but practically fine.
  fromIntegral $ diffDays streakEnd streakBegin + 1

streakIsCurrent :: Streak -> Day -> Bool
streakIsCurrent Streak {..} d = streakEnd == d

data RangeSum
  = CompleteSum Word
  | PartialSumBegin Word
  | PartialSumEnd Word
  | PartialSumBoth Word
  | NoSum
  deriving (Show, Eq, Generic)

instance Validity RangeSum

-- O(n) in the number of entries between these two days
entryMapRangeSum :: Bool -> EntryMap -> Day -> Day -> RangeSum
entryMapRangeSum goalBoolean (EntryMap em') beginDay endDay =
  let (_, geBeginMap) = M.split (addDays (-1) beginDay) em' -- O(Log n)
      (em, _) = M.split (addDays 1 endDay) geBeginMap -- O(Log n)
      count :: Word -> Word
      count w =
        if goalBoolean
          then if w > 0 then 1 else 0
          else w
   in case (,) <$> M.lookupMin em' <*> M.lookupMax em' of
        Nothing -> NoSum -- No entries, no amounts known
        Just ((firstDay, _), (lastDay, _)) ->
          let c = sum $ M.map count em
           in case (beginDay < firstDay, endDay > lastDay) of
                (True, True) -> PartialSumBoth c
                (True, False) -> PartialSumBegin c
                (False, True) -> PartialSumEnd c
                (False, False) -> CompleteSum c

-- For positive habits, a goal is met if the total amount over the last *denominator* days
-- is more than *numerator*.
-- For negative habits, a goal is met if the total amount over the last *denominator* days
-- is _less_ than *numerator*.
-- O(n) in the number of entries
entryMapGoalMet :: Goal -> Day -> EntryMap -> Maybe Bool -- Nothing means not enough data
entryMapGoalMet Goal {..} endDay em =
  let beginDay = addDays (- fromIntegral (goalDenominator - 1)) endDay
   in case entryMapRangeSum goalBoolean em beginDay endDay of
        NoSum -> Nothing
        PartialSumBegin w -> case goalType of
          PositiveHabit ->
            if w >= goalNumerator
              then Just True -- If we already achieve our goal earlier, we can work with partial data.
              else Nothing
          NegativeHabit -> Nothing
        PartialSumEnd w -> case goalType of
          PositiveHabit ->
            if w >= goalNumerator
              then Just True -- If we already achieve our goal earlier, we can work with partial data.
              else Nothing
          NegativeHabit ->
            if w <= goalNumerator
              then Just True -- We assume zero at the end
              else Nothing
        PartialSumBoth w -> case goalType of
          PositiveHabit ->
            if w >= goalNumerator
              then Just True -- If we already achieve our goal earlier, we can work with partial data.
              else Nothing
          NegativeHabit -> Nothing
        CompleteSum w -> Just $ case goalType of
          PositiveHabit -> w >= goalNumerator
          NegativeHabit -> w <= goalNumerator

-- [Note: Complexity]
-- This is currently linear in the number of days between the first and the last entry.
-- FIXME: I _think_ that can be sped up but it's not as simple as it seems.
entryMapStreaks :: Goal -> EntryMap -> Day -> [Streak]
entryMapStreaks g@Goal {..} em@(EntryMap m) endDay =
  case M.lookupMin m of
    Nothing -> [] -- No entries, no streaks
    Just (beginDay, _) ->
      let ends = [beginDay .. endDay]
          goalMets = map (\end -> (end, entryMapGoalMet g end em)) ends
       in buildStreaks goalMets

buildStreaks :: [(Day, Maybe Bool)] -> [Streak]
buildStreaks = go Nothing
  where
    go :: Maybe Streak -> [(Day, Maybe Bool)] -> [Streak]
    go Nothing [] = []
    go (Just s) [] = [s]
    go mcur ((d, met) : rest) = case mcur of
      Nothing -> case met of
        Just True -> go (Just (Streak d d)) rest
        _ -> go Nothing rest
      Just s -> case met of
        Just True -> go (Just $ s {streakEnd = d}) rest
        Just False -> s : go Nothing rest
        Nothing -> s : go Nothing rest

entryMapLongestStreak :: Goal -> EntryMap -> Day -> Maybe Streak
entryMapLongestStreak g em today = maximumByMay (comparing streakDays) $ entryMapStreaks g em today

entryMapLatestStreak :: Goal -> EntryMap -> Day -> Maybe Streak
entryMapLatestStreak g em today = lastMay $ entryMapStreaks g em today
