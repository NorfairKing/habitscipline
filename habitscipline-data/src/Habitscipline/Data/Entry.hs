{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Habitscipline.Data.Entry where

import Data.Aeson
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
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
      { streakBegin :: Day,
        streakEnd :: Day
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity Streak where
  validate s@Streak {..} =
    mconcat
      [ genericValidate s,
        declare "The end is on or after the begin" $ streakEnd >= streakBegin
      ]

-- O(n) in the number of entries between these two days
entryMapRangeSum :: Bool -> EntryMap -> Day -> Day -> Maybe Word
entryMapRangeSum habitBoolean (EntryMap em') beginDay endDay =
  let (_, geBeginMap) = M.split (addDays (-1) beginDay) em' -- O(Log n)
      (em, _) = M.split (addDays 1 endDay) geBeginMap -- O(Log n)
      count :: Word -> Word
      count w =
        if habitBoolean
          then if w > 0 then 1 else 0
          else w
   in case M.lookupMin em' of
        Nothing -> Nothing -- No entries, no amounts known
        Just (firstDay, _) ->
          if beginDay < firstDay
            then Nothing
            else Just $ sum $ M.map count em

-- For positive habits, a goal is met if the total amount over the last *denominator* days
-- is more than *numerator*.
-- For negative habits, a goal is met if the total amount over the last *denominator* days
-- is _less_ than *numerator*.
-- O(n) in the number of entries
entryMapGoalMet :: HabitType -> Bool -> Goal -> Day -> EntryMap -> Maybe Bool
entryMapGoalMet ht habitBoolean Goal {..} endDay em =
  let cmp = case ht of
        PositiveHabit -> (>=)
        NegativeHabit -> (<=)
      beginDay = addDays (- fromIntegral goalDenominator) endDay
   in (`cmp` goalNumerator) <$> entryMapRangeSum habitBoolean em beginDay endDay

-- [Note: Complexity]
-- This is currently linear in the number of days between the first and the last entry.
-- FIXME: I _think_ that can be sped up but it's not as simple as it seems because Streaks can overlap.
-- Indeed, take a positive goal of 2/4 and the entries 1,0,0,1,0,0,1.
-- Then there are two streaks that overlap on the middle day.
entryMapStreaks :: HabitType -> Bool -> Goal -> EntryMap -> [Streak]
entryMapStreaks ht b g@Goal {..} em@(EntryMap m) =
  case M.lookupMax m of
    Nothing -> [] -- No entries, no streaks
    Just (endDay, _) ->
      case M.lookupMin m of
        Nothing -> []
        Just (beginDay, _) ->
          let ends = [beginDay .. endDay]
           in mapMaybe
                ( \end -> do
                    goalMet <- entryMapGoalMet ht b g end em
                    if goalMet
                      then
                        let begin = addDays (- fromIntegral goalDenominator) end
                         in pure $ Streak begin end
                      else Nothing
                )
                ends

entryMapLongestStreak :: HabitType -> Bool -> Goal -> EntryMap -> Maybe Streak
entryMapLongestStreak ht b g em = maximumMay $ entryMapStreaks ht b g em

entryMapLatestStreak :: HabitType -> Bool -> Goal -> EntryMap -> Maybe Streak
entryMapLatestStreak ht b g em = lastMay $ entryMapStreaks ht b g em
