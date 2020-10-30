{-# LANGUAGE TypeApplications #-}

module Habitscipline.API.DataSpec
  ( spec,
  )
where

import Habitscipline.API.Data
import Habitscipline.API.Data.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @RegistrationForm
  genValidSpec @LoginForm
