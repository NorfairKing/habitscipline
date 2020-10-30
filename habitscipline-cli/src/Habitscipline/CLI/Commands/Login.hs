module Habitscipline.CLI.Commands.Login where

import Habitscipline.CLI.Commands.Import

login :: C ()
login = withClient $ \cenv -> withLogin cenv $ \_ ->
  pure ()
