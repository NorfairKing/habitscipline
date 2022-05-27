{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Habitscipline.CLI.OptParse
  ( getInstructions,
    Instructions (..),
    Dispatch (..),
    Settings (..),
    EntrySettings (..),
    getDefaultClientDatabase,
    getDefaultDataDir,
    getDefaultConfigFile,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Control.Arrow
import Control.Monad.Logger
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Yaml (FromJSON, ToJSON)
import qualified Env
import GHC.Generics (Generic)
import Habitscipline.API.Server.Data
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO
import Servant.Client

data Instructions
  = Instructions Dispatch Settings
  deriving (Show, Generic)

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

-- | A product type for the settings that are common across commands
data Settings = Settings
  { settingBaseUrl :: Maybe BaseUrl,
    settingUsername :: Maybe Username,
    settingPassword :: Maybe Text,
    settingDbFile :: Path Abs File,
    settingLogLevel :: LogLevel
  }
  deriving (Show, Generic)

-- | A sum type for the commands and their specific settings
data Dispatch
  = DispatchRegister
  | DispatchLogin
  | DispatchSync
  | DispatchEntry EntrySettings
  deriving (Show, Generic)

data EntrySettings = EntrySettings
  { entrySettingHabit :: Text,
    entrySettingAmount :: Word,
    entrySettingDay :: Day
  }
  deriving (Show, Generic)

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments cmd Flags {..}) Environment {..} mConf = do
  -- This is a typical way to combine a setting.
  --
  -- We choose the first of the supplied flag, environment variable or configuration field,
  -- or default value if none of the those were supplied.
  let settingBaseUrl = flagBaseUrl <|> envBaseUrl <|> mc configBaseUrl
  let settingUsername = flagUsername <|> envUsername <|> mc configUsername
  let settingPassword = flagPassword <|> envPassword <|> mc configPassword
  settingDbFile <- case flagDbFile <|> envDbFile <|> mc configDbFile of
    Nothing -> getDefaultClientDatabase
    Just dbf -> resolveFile' dbf
  let settingLogLevel = fromMaybe LevelWarn $ flagLogLevel <|> envLogLevel <|> mc configLogLevel
  let sets = Settings {..}
  disp <-
    -- Resolve the command-specific settings for each command
    case cmd of
      CommandRegister -> pure DispatchRegister
      CommandLogin -> pure DispatchLogin
      CommandSync -> pure DispatchSync
      CommandEntry EntryFlags {..} -> do
        let entrySettingHabit = entryFlagHabit
        let entrySettingAmount = entryFlagAmount
        entrySettingDay <- case entryFlagDay of
          Nothing -> utctDay <$> getCurrentTime
          Just d -> pure d
        pure $ DispatchEntry EntrySettings {..}
  pure $ Instructions disp sets
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

getDefaultClientDatabase :: IO (Path Abs File)
getDefaultClientDatabase = do
  dataDir <- getDefaultDataDir
  resolveFile dataDir "habit-data.sqlite3"

getDefaultDataDir :: IO (Path Abs Dir)
getDefaultDataDir = getXdgDir XdgData (Just [reldir|habitscipline|])

getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|habitscipline|])
  resolveFile xdgConfigDir "config.yaml"

data Configuration = Configuration
  { configBaseUrl :: Maybe BaseUrl,
    configUsername :: Maybe Username,
    configPassword :: Maybe Text,
    configDbFile :: Maybe FilePath,
    configLogLevel :: Maybe LogLevel
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldOrNullWith "server-url" (bimapCodec (left show . parseBaseUrl) showBaseUrl codec) "Server base url" .= configBaseUrl
        <*> optionalFieldOrNull "username" "Server account username" .= configUsername
        <*> optionalFieldOrNull "password" "Server account password" .= configPassword
        <*> optionalFieldOrNull "database" "The path to the database" .= configDbFile
        <*> optionalFieldOrNull "log-level" "The minimal severity for log messages" .= configLogLevel

instance HasCodec LogLevel where
  codec =
    stringConstCodec
      [ (LevelDebug, "Debug"),
        (LevelInfo, "Info"),
        (LevelWarn, "Warn"),
        (LevelError, "Error")
      ]

-- | Get the configuration
--
-- We use the flags and environment because they can contain information to override where to look for the configuration files.
-- We return a 'Maybe' because there may not be a configuration file.
getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> getDefaultConfigFile >>= readYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

-- | What we find in the configuration variable.
--
-- Do nothing clever here, just represent the relevant parts of the environment.
-- For example, use 'Text', not 'SqliteConfig'.
data Environment = Environment
  { envConfigFile :: Maybe FilePath,
    envBaseUrl :: Maybe BaseUrl,
    envUsername :: Maybe Username,
    envPassword :: Maybe Text,
    envDbFile :: Maybe FilePath,
    envLogLevel :: Maybe LogLevel
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

-- | The 'envparse' parser for the 'Environment'
environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "HABITSCIPLINE_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (mE <> Env.help "Config file")
      <*> Env.var (fmap Just . maybe (Left $ Env.unread "unable to parse base url") Right . parseBaseUrl) "SERVER_URL" (mE <> Env.help "Server base url")
      <*> Env.var (fmap Just . left Env.unread . parseUsernameOrErr . T.pack) "USERNAME" (mE <> Env.help "Server account username")
      <*> Env.var (fmap Just . Env.str) "PASSWORD" (mE <> Env.help "Server account password")
      <*> Env.var (fmap Just . Env.str) "DATABASE" (mE <> Env.help "Path to the database")
      <*> Env.var (fmap Just . Env.auto) "LOG_LEVEL" (mE <> Env.help "Minimal severity for log messages")
  where
    mE = Env.def Nothing

-- | The combination of a command with its specific flags and the flags for all commands
data Arguments
  = Arguments Command Flags
  deriving (Show, Generic)

-- | Get the command-line arguments
getArguments :: IO Arguments
getArguments = customExecParser prefs_ argParser

-- | The 'optparse-applicative' parsing preferences
prefs_ :: OptParse.ParserPrefs
prefs_ =
  -- I like these preferences. Use what you like.
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

-- | The @optparse-applicative@ parser for 'Arguments'
argParser :: OptParse.ParserInfo Arguments
argParser =
  OptParse.info
    (OptParse.helper <*> parseArgs)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    -- Show the variables from the environment that we parse and the config file format
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (TE.decodeUtf8 (renderColouredSchemaViaCodec @Configuration))
        ]

parseArgs :: OptParse.Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

-- | A sum type for the commands and their specific arguments
data Command
  = CommandRegister
  | CommandLogin
  | CommandSync
  | CommandEntry EntryFlags
  deriving (Show, Generic)

parseCommand :: OptParse.Parser Command
parseCommand =
  OptParse.hsubparser $
    mconcat
      [ OptParse.command "register" parseCommandRegister,
        OptParse.command "login" parseCommandLogin,
        OptParse.command "sync" parseCommandSync,
        OptParse.command "entry" parseCommandEntry
      ]

parseCommandRegister :: OptParse.ParserInfo Command
parseCommandRegister = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Register the user"
    parser = pure CommandRegister

parseCommandLogin :: OptParse.ParserInfo Command
parseCommandLogin = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Log the user in"
    parser = pure CommandLogin

parseCommandSync :: OptParse.ParserInfo Command
parseCommandSync = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Synchronise the habit database"
    parser = pure CommandSync

data EntryFlags = EntryFlags
  { entryFlagHabit :: Text,
    entryFlagAmount :: Word,
    entryFlagDay :: Maybe Day
  }
  deriving (Show)

parseCommandEntry :: OptParse.ParserInfo Command
parseCommandEntry = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Set an entry"
    parser =
      CommandEntry
        <$> ( EntryFlags
                <$> strArgument (mconcat [metavar "HABIT", help "The habit: UUID or name"])
                <*> argument auto (mconcat [metavar "AMOUNT", help "The amount for the entry"])
                <*> optional (option auto (mconcat [metavar "DAY", long "day", help "The day to set the entry for"]))
            )

-- | The flags that are common across commands.
data Flags = Flags
  { flagConfigFile :: Maybe FilePath,
    flagBaseUrl :: Maybe BaseUrl,
    flagUsername :: Maybe Username,
    flagPassword :: Maybe Text,
    flagDbFile :: Maybe FilePath,
    flagLogLevel :: Maybe LogLevel
  }
  deriving (Show, Eq, Generic)

-- | The 'optparse-applicative' parser for the 'Flags'.
parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Give the path to an altenative config file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( option
          (maybeReader parseBaseUrl)
          ( mconcat
              [ long "server-url",
                help "Server base url"
              ]
          )
      )
    <*> optional
      ( option
          (eitherReader $ parseUsernameOrErr . T.pack)
          ( mconcat
              [ long "username",
                help "Server account username",
                metavar "USERNAME"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "password",
                help "Server account password",
                metavar "PASSWORD"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "database",
                help "Path to the database",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "log-level",
                help "Minimal severity level for log messages",
                metavar "LOG_LEVEL"
              ]
          )
      )
