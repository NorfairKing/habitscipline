{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Habitscipline.API.Server where

import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Database.Persist.Sql
import Database.Persist.Sqlite
import Habitscipline.API as API
import Habitscipline.API.Server.Env
import Habitscipline.API.Server.Handler
import Habitscipline.API.Server.OptParse
import Habitscipline.API.Server.SigningKey
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Path
import Servant.Auth.Server
import Servant.Server.Generic

habitsciplineAPIServer :: IO ()
habitsciplineAPIServer = do
  Settings {..} <- getSettings
  runStderrLoggingT $ withSqlitePool (T.pack (fromAbsFile settingDbFile)) 1 $ \pool -> do
    runSqlPool (runMigration serverMigration) pool
    liftIO $ do
      jwk <- loadSigningKey settingSigningKeyFile
      let serverEnv =
            Env
              { envConnectionPool = pool,
                envCookieSettings = defaultCookieSettings,
                envJWTSettings = defaultJWTSettings jwk
              }
      Warp.run settingPort $ habitsciplineAPIServerApp serverEnv

habitsciplineAPIServerApp :: Env -> Wai.Application
habitsciplineAPIServerApp env =
  genericServeTWithContext
    (flip runReaderT env)
    habitsciplineHandlers
    (habitsciplineContext env)

habitsciplineContext :: Env -> Context '[CookieSettings, JWTSettings]
habitsciplineContext Env {..} = envCookieSettings :. envJWTSettings :. EmptyContext

habitsciplineHandlers :: HabitsciplineRoutes (AsServerT H)
habitsciplineHandlers =
  HabitsciplineRoutes
    { postRegister = handlePostRegister,
      postLogin = handlePostLogin,
      postSync = protected handlePostSync
    }

protected :: ThrowAll m => (authCookie -> m) -> AuthResult authCookie -> m
protected func (Authenticated authCookie) = func authCookie
protected _ _ = throwAll err401
