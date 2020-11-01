{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Habitscipline.Data.Entry where

import Data.Aeson
import Data.Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)

data Entry
  = Entry
      { entryDay :: !Day,
        entryAmount :: !Word
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity Entry

instance FromJSON Entry where
  parseJSON = withObject "Entry" $ \o ->
    Entry
      <$> o .: "day"
      <*> o .: "amount"

instance ToJSON Entry where
  toJSON Entry {..} =
    object
      [ "day" .= entryDay,
        "amount" .= entryAmount
      ]
