{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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

data RegistrationForm = RegistrationForm
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

data LoginForm = LoginForm
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

data AuthCookie = AuthCookie
  { authCookieUsername :: Username
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AuthCookie

instance ToJSON AuthCookie

instance FromJWT AuthCookie

instance ToJWT AuthCookie

data SyncRequest = SyncRequest
  { syncRequestHabitSyncRequest :: Mergeful.SyncRequest ClientHabitId ServerHabitId Habit,
    syncRequestEntrySyncRequest :: Mergeful.SyncRequest ClientEntryId ServerEntryId Entry
  }
  deriving (Show, Eq, Generic)

instance Validity SyncRequest

instance FromJSON SyncRequest where
  parseJSON =
    withObject "SyncRequest" $ \o ->
      SyncRequest <$> o .: "habit" <*> o .: "entry"

instance ToJSON SyncRequest where
  toJSON SyncRequest {..} =
    object
      [ "habit" .= syncRequestHabitSyncRequest,
        "entry" .= syncRequestEntrySyncRequest
      ]

data SyncResponse = SyncResponse
  { syncResponseHabitSyncResponse :: Mergeful.SyncResponse ClientHabitId ServerHabitId Habit,
    syncResponseEntrySyncResponse :: Mergeful.SyncResponse ClientEntryId ServerEntryId Entry
  }
  deriving (Show, Eq, Generic)

instance Validity SyncResponse

instance FromJSON SyncResponse where
  parseJSON =
    withObject "SyncResponse" $ \o ->
      SyncResponse <$> o .: "habit" <*> o .: "entry"

instance ToJSON SyncResponse where
  toJSON SyncResponse {..} =
    object
      [ "habit" .= syncResponseHabitSyncResponse,
        "entry" .= syncResponseEntrySyncResponse
      ]

instance (PersistEntity a, ToBackendKey SqlBackend a) => ToJSONKey (Key a) where
  toJSONKey = contramap fromSqlKey toJSONKey

instance (PersistEntity a, ToBackendKey SqlBackend a) => FromJSONKey (Key a) where
  fromJSONKey = toSqlKey <$> fromJSONKey
