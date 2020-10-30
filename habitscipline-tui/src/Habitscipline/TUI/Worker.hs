module Habitscipline.TUI.Worker where

import Brick.BChan
import Control.Monad
import Control.Monad.IO.Class
import Habitscipline.TUI.Env

tuiWorker :: BChan Request -> BChan Response -> W ()
tuiWorker reqChan respChan = forever $ do
  req <- liftIO $ readBChan reqChan
  resp <- case req of
    Request -> pure Response
  liftIO $ writeBChan respChan resp
