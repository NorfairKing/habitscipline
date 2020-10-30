module Habitscipline.Client
  ( module Habitscipline.Client,
    module Habitscipline.API,
    module X,
  )
where

import Habitscipline.API
import Servant.API as X
import Servant.Auth.Client as X
import Servant.Client as X
import Servant.Client.Generic

habitsciplineClient :: HabitsciplineRoutes (AsClientT ClientM)
habitsciplineClient = genericClient
