module Habitscipline.TUI.Env where

import Control.Monad.Reader

data Env
  = Env
      {
      }
  deriving (Show, Eq, Ord)

type W = ReaderT Env IO

data Request = Request

data Response = Response
