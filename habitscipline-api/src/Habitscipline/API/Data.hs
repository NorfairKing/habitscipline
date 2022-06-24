{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Habitscipline.API.Data where

import Autodocodec
import Data.Aeson (FromJSON, FromJSONKey (..), ToJSON, ToJSONKey (..))
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
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec RegistrationForm)

instance Validity RegistrationForm where
  validate rf@RegistrationForm {..} =
    mconcat
      [ genericValidate rf,
        declare "The password is nonempty" $ not $ T.null registrationFormPassword
      ]

instance HasCodec RegistrationForm where
  codec =
    object "RegistrationForm" $
      RegistrationForm
        <$> requiredField "username" "user name" .= registrationFormUsername
        <*> requiredField "password" "password" .= registrationFormPassword

data LoginForm = LoginForm
  { loginFormUsername :: Username,
    loginFormPassword :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec LoginForm)

instance Validity LoginForm

instance HasCodec LoginForm where
  codec =
    object "LoginForm" $
      LoginForm
        <$> requiredField "username" "user name" .= loginFormUsername
        <*> requiredField "password" "password" .= loginFormPassword

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
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec SyncRequest)

instance Validity SyncRequest

instance HasCodec SyncRequest where
  codec =
    object "SyncRequest" $
      SyncRequest
        <$> requiredField "habit" "habit sync request" .= syncRequestHabitSyncRequest
        <*> requiredField "entry" "entry sync request" .= syncRequestEntrySyncRequest

data SyncResponse = SyncResponse
  { syncResponseHabitSyncResponse :: Mergeful.SyncResponse ClientHabitId ServerHabitId Habit,
    syncResponseEntrySyncResponse :: Mergeful.SyncResponse ClientEntryId ServerEntryId Entry
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec SyncResponse)

instance Validity SyncResponse

instance HasCodec SyncResponse where
  codec =
    object "SyncResponse" $
      SyncResponse
        <$> requiredField "habit" "habit sync response" .= syncResponseHabitSyncResponse
        <*> requiredField "entry" "entry sync response" .= syncResponseEntrySyncResponse

instance ToBackendKey SqlBackend a => HasCodec (Key a) where
  codec = dimapCodec toSqlKey fromSqlKey codec

instance (PersistEntity a, ToBackendKey SqlBackend a) => ToJSONKey (Key a) where
  toJSONKey = contramap fromSqlKey toJSONKey

instance (PersistEntity a, ToBackendKey SqlBackend a) => FromJSONKey (Key a) where
  fromJSONKey = toSqlKey <$> fromJSONKey
