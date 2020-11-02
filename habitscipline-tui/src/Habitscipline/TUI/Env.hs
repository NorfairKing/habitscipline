module Habitscipline.TUI.Env where

import Control.Monad.Reader
import Data.Map (Map)
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
  = RequestHistory
  | RequestHabits
  | RequestSetEntry !Entry
  | RequestCreateHabit !Habit

data Response
  = ResponseHistory !(Map Habit EntryMap)
  | ResponseHabits ![Habit]
