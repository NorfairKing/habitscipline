{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Habitscipline.API.Data where

import Data.Aeson
import Data.Functor.Contravariant
import qualified Data.Mergeful as Mergeful
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import Habitscipline.API.Server.Data
import Habitscipline.Client.Data
import Habitscipline.Data
import Servant.API.Generic
import Servant.Auth.Server

data RegistrationForm
  = RegistrationForm
      { registrationFormUsername :: Username,
        registrationFormPassword :: Text
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity RegistrationForm where
  validate rf@RegistrationForm {..} =
    mconcat
      [ genericValidate rf,
        declare "The password is nonempty" $ not $ T.null registrationFormPassword
      ]

instance ToJSON RegistrationForm where
  toJSON RegistrationForm {..} =
    object
      [ "name" .= registrationFormUsername,
        "password" .= registrationFormPassword
      ]

instance FromJSON RegistrationForm where
  parseJSON =
    withObject "RegistrationForm" $ \o ->
      RegistrationForm <$> o .: "name" <*> o .: "password"

data LoginForm
  = LoginForm
      { loginFormUsername :: Username,
        loginFormPassword :: Text
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity LoginForm

instance FromJSON LoginForm where
  parseJSON = withObject "LoginForm" $ \o ->
    LoginForm <$> o .: "username" <*> o .: "password"

instance ToJSON LoginForm where
  toJSON LoginForm {..} =
    object
      [ "username" .= loginFormUsername,
        "password" .= loginFormPassword
      ]

data AuthCookie
  = AuthCookie
      { authCookieUsername :: Username
      }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AuthCookie

instance ToJSON AuthCookie

instance FromJWT AuthCookie

instance ToJWT AuthCookie

type SyncRequest = Mergeful.SyncRequest ClientHabitId ServerHabitId Habit

type SyncResponse = Mergeful.SyncResponse ClientHabitId ServerHabitId Habit

instance (PersistEntity a, ToBackendKey SqlBackend a) => ToJSONKey (Key a) where
  toJSONKey = contramap fromSqlKey toJSONKey

instance (PersistEntity a, ToBackendKey SqlBackend a) => FromJSONKey (Key a) where
  fromJSONKey = toSqlKey <$> fromJSONKey
