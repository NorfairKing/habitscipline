module Habitscipline.CLISpec (spec) where

import qualified Data.Text as T
import Habitscipline.API.Data
import Habitscipline.API.Server.Data
import Habitscipline.API.Server.TestUtils
import Habitscipline.CLI
import Path
import Path.IO
import Servant.Client
import System.Environment
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = serverSpec $
  describe "Habitscipline CLI" $
    it "'just works'" $
      \cenv -> forAllValid $ \rf -> withSystemTempDir "habitscipline-cli" $ \tdir -> do
        setEnv "HABITSCIPLINE_SERVER_URL" $ showBaseUrl $ baseUrl cenv
        setEnv "HABITSCIPLINE_USERNAME" $ T.unpack $ usernameText $ registrationFormUsername rf
        setEnv "HABITSCIPLINE_PASSWORD" $ T.unpack $ registrationFormPassword rf
        dbFile <- resolveFile tdir "habitscipline-client.sqlite3"
        setEnv "HABITSCIPLINE_DATABASE" $ fromAbsFile dbFile
        configFile <- resolveFile tdir "habitscipline-config.yaml"
        setEnv "HABITSCIPLINE_CONFIG_FILE" $ fromAbsFile configFile
        let testHabitscipline args = withArgs args habitsciplineCLI
        testHabitscipline ["register"]
        testHabitscipline ["login"]
        testHabitscipline ["sync"]
