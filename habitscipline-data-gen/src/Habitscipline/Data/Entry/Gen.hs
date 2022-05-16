{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Habitscipline.Data.Entry.Gen where

import Control.Monad
import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID.Typed ()
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import Habitscipline.Data.Entry
import Test.QuickCheck

instance GenValid Entry where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid EntryMap where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = sized $ \s -> do
    -- We only generate entry maps where the days are quite close together
    -- for complexity reasons, but this also more realistic.
    -- See also [Note: Complexity]
    beginDay <- genValid
    interval <- upTo (s * 2)
    let endDay = addDays (fromIntegral interval) beginDay
    let days = [beginDay .. endDay]
    unEntryMap <- fmap (M.fromList . catMaybes) $
      forM days $ \d ->
        oneof
          [ pure Nothing,
            do
              w <- choose (0, s)
              pure $ Just (d, fromIntegral w)
          ]
    pure EntryMap {..}
