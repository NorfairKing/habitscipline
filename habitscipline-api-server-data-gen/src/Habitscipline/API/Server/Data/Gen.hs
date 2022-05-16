{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Habitscipline.API.Server.Data.Gen where

import Control.Monad
import qualified Data.ByteString as SB
import Data.GenValidity
import Data.GenValidity.Mergeful ()
import Data.GenValidity.Persist ()
import Data.GenValidity.Text ()
import Data.Password.Bcrypt
import Habitscipline.API.Server.Data
import Habitscipline.Data.Gen ()
import Test.QuickCheck

instance GenValid (Salt Bcrypt) where
  genValid = Salt <$> (SB.pack <$> replicateM 32 (choose (0, 255)))
  shrinkValid _ = [] -- No use

instance GenValid Password where
  genValid = mkPassword <$> genValid
  shrinkValid _ = [] -- No use

instance GenValid PassHash where
  genValid = hashPasswordWithSalt 4 <$> genValid <*> genValid
  shrinkValid _ = [] -- No use

instance GenValid Username

instance GenValid User

instance GenValid ServerHabit
