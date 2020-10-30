{-# OPTIONS_GHC -fno-warn-orphans #-}

module Habitscipline.API.Server.Data.Gen where

import Control.Monad
import qualified Data.ByteString as SB
import Data.GenValidity
import Data.GenValidity.Mergeful ()
import Data.GenValidity.Persist ()
import Data.GenValidity.Text ()
import Data.Password
import Habitscipline.API.Server.Data
import Habitscipline.Data.Gen ()
import Test.QuickCheck

instance GenValid Salt where
  genValid = Salt <$> (SB.pack <$> replicateM 32 (choose (0, 255)))
  shrinkValid _ = [] -- No use

instance GenValid Pass where
  genValid = mkPass <$> genValid
  shrinkValid _ = [] -- No use

instance GenValid PassHash where
  genValid = hashPassWithSalt <$> genValid <*> (mkPass <$> genValid)
  shrinkValid _ = [] -- No use

instance GenValid Username where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid User where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ServerHabit where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
