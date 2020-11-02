{-# LANGUAGE TypeApplications #-}

module Habitscipline.Data.EntrySpec
  ( spec,
  )
where

import Habitscipline.Data.Entry
import Habitscipline.Data.Entry.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Entry
  jsonSpecOnValid @Entry
  genValidSpec @EntryMap
  jsonSpecOnValid @EntryMap
