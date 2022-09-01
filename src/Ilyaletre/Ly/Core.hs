{-# LANGUAGE DuplicateRecordFields #-}

module Ilyaletre.Ly.Core
  ( Id,
    Lane (..),
    Priority (..),
    Task (..),
    TaskView (..),
    TaskRequest (..),
    MonadTask (..),
  )
where

import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import Database.SQLite.Simple.ToRow (ToRow (..))

type Id = Int64

data Lane = Lane
  { id :: Id,
    name :: T.Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

instance FromRow Lane where
  fromRow = Lane <$> field <*> field <*> field <*> field

data Priority = Priority
  { id :: Id,
    name :: T.Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

instance FromRow Priority where
  fromRow = Priority <$> field <*> field <*> field <*> field

data Task = Task
  { id :: Id,
    summary :: T.Text,
    estimate :: Int64,
    laneId :: Id,
    priorityId :: Id,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

instance FromRow Task where
  fromRow = Task <$> field <*> field <*> field <*> field <*> field <*> field <*> field

-- | Taskの表示用データ構造
data TaskView = TaskView
  { id :: Id,
    summary :: T.Text,
    estimate :: Int64,
    laneId :: Id,
    lane :: T.Text,
    priorityId :: Id,
    priority :: T.Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

instance FromRow TaskView where
  fromRow = TaskView <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data TaskRequest
  = TaskAdd
      { summary :: T.Text,
        estimate :: Int64,
        laneId :: Id,
        priorityId :: Id
      }
  | TaskUpdate
      { id :: Id,
        summary :: T.Text,
        estimate :: Int64,
        laneId :: Id,
        priorityId :: Id
      }

instance ToRow TaskRequest where
  toRow (TaskAdd c0 c1 c2 c3) = toRow (c0, c1, c2, c3)
  toRow (TaskUpdate c0 c1 c2 c3 c4) = toRow (c0, c1, c2, c3, c4)

class (Monad m) => MonadTask m where
  getTask :: Id -> m TaskView
  saveTask :: TaskRequest -> m TaskView

data Todo = Todo
  { id :: Id,
    date :: UTCTime,
    note :: T.Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field <*> field <*> field

data Timer = Timer
  { id :: Id,
    label :: T.Text,
    timerTypeId :: Id,
    startedAt :: UTCTime,
    duration :: Int64
  }

instance FromRow Timer where
  fromRow = Timer <$> field <*> field <*> field <*> field <*> field
