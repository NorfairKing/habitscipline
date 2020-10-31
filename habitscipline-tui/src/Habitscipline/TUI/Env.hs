module Habitscipline.TUI.Env where

import Control.Monad.Reader
import Database.Persist.Sql
import Habitscipline.Data

data Env
  = Env
      { envConnectionPool :: ConnectionPool
      }

type W = ReaderT Env IO

runDB :: SqlPersistT IO a -> W a
runDB func = do
  pool <- asks envConnectionPool
  liftIO $ runSqlPool func pool

data Request
  = RequestHabits
  | RequestCreateHabit Habit

data Response = ResponseHabits [Habit]
