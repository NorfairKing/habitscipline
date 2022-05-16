module Habitscipline.API.Server.Handler.SyncSpec (spec) where

import Control.Monad
import Habitscipline.API
import Habitscipline.API.Data.Gen ()
import Habitscipline.API.Server.TestUtils
import Habitscipline.Client
import Habitscipline.Data.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = serverSpec $
  describe "PostSync" $
    it "does not crash" $
      \cenv -> forAllValid $ \req -> withAnyNewUser cenv $ \token ->
        void $ testClientOrErr cenv $ postSync habitsciplineClient token req
