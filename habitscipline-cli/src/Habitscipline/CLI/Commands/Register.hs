{-# LANGUAGE RecordWildCards #-}

module Habitscipline.CLI.Commands.Register where

import Habitscipline.CLI.Commands.Import

register :: C ()
register = withClient $ \cenv -> do
  registrationFormUsername <- getEnvUsername
  registrationFormPassword <- getEnvPassword
  let rf = RegistrationForm {..}
  NoContent <- runClientOrDie cenv $ postRegister habitsciplineClient rf
  pure ()
