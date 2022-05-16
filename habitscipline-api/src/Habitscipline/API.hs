{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Habitscipline.API where

import Data.Proxy
import Data.Text (Text)
import Data.Validity.Text ()
import Habitscipline.API.Data
import Servant.API
import Servant.API.Generic
import Servant.Auth

habitsciplineAPI :: Proxy HabitsciplineAPI
habitsciplineAPI = Proxy

type HabitsciplineAPI = ToServantApi HabitsciplineRoutes

data HabitsciplineRoutes route = HabitsciplineRoutes
  { postRegister :: !(route :- PostRegister),
    postLogin :: !(route :- PostLogin),
    postSync :: !(route :- PostSync)
  }
  deriving (Generic)

type PostRegister =
  "register"
    :> ReqBody '[JSON] RegistrationForm
    :> Post '[JSON] NoContent

type PostLogin =
  "login"
    :> ReqBody '[JSON] LoginForm
    :> PostNoContent '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)

type ProtectAPI = Auth '[JWT] AuthCookie

type PostSync =
  ProtectAPI
    :> "sync"
    :> ReqBody '[JSON] SyncRequest
    :> Get '[JSON] SyncResponse
