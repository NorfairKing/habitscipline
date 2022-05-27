{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Habitscipline.API.Server.Data.Username where

import Autodocodec
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import YamlParse.Applicative

newtype Username = Username
  { usernameText :: Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSONKey, ToJSONKey)
  deriving (FromJSON, ToJSON) via (Autodocodec Username)

instance Validity Username where
  validate (Username t) =
    mconcat
      [ check (not (T.null t)) "The username is not empty.",
        check (T.length t >= 3) "The username is at least three characters long."
      ]

instance HasCodec Username where
  codec = bimapCodec parseUsernameOrErr usernameText codec

instance PersistField Username where
  toPersistValue = toPersistValue . usernameText
  fromPersistValue pv = do
    t <- fromPersistValue pv
    case parseUsername t of
      Nothing -> Left "Text isn't a valid username"
      Just un -> Right un

instance PersistFieldSql Username where
  sqlType _ = SqlString

instance YamlSchema Username where
  yamlSchema = eitherParser parseUsernameOrErr yamlSchema

parseUsername :: Text -> Maybe Username
parseUsername = constructValid . Username

parseUsernameOrErr :: Text -> Either String Username
parseUsernameOrErr = prettyValidate . Username
