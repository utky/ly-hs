{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Ilyaletre.Ly.Core
  ( Id,
    Lane (..),
    Priority (..),
    Task (..),
    TaskView (..),
    AddTaskRequest (..),
    UpdateTaskRequest (..),
    TaskStore (..),
    TaskSearch (..),
  )
where

import Data.Aeson
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import Database.SQLite.Simple.ToRow (ToRow (..))
import GHC.Generics

-- Type

type Id = Int64

-- Lane

data Lane = Lane
  { id' :: Id,
    name :: T.Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Eq, Show)

instance FromRow Lane where
  fromRow =
    Lane
      <$> field
      <*> field
      <*> field
      <*> field

-- Priority

data Priority = Priority
  { id' :: Id,
    name :: T.Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Eq, Show)

instance FromRow Priority where
  fromRow =
    Priority
      <$> field
      <*> field
      <*> field
      <*> field

-- Task

data Task = Task
  { id' :: Id,
    summary :: T.Text,
    estimate :: Int64,
    laneId :: Id,
    priorityId :: Id,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Eq, Show)

instance FromRow Task where
  fromRow =
    Task
      <$> field -- id
      <*> field -- summary
      <*> field -- estimate
      <*> field -- laneId
      <*> field -- priorityId
      <*> field -- createdAt
      <*> field -- updatedAt

-- | Taskの表示用データ構造
data TaskView = TaskView
  { id' :: Id,
    summary :: T.Text,
    estimate :: Int64,
    laneId :: Id,
    lane :: T.Text,
    priorityId :: Id,
    priority :: T.Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON TaskView where
  toEncoding = genericToEncoding defaultOptions

instance FromRow TaskView where
  fromRow =
    TaskView
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data AddTaskRequest = AddTaskRequest
  { summary :: T.Text,
    estimate :: Int64,
    laneId :: Id,
    priorityId :: Id
  }
  deriving (Eq, Show, Generic)

instance FromJSON AddTaskRequest

instance ToRow AddTaskRequest where
  toRow (AddTaskRequest c1 c2 c3 c4) =
    toRow
      ( c1,
        c2,
        c3,
        c4
      )

data UpdateTaskRequest = UpdateTaskRequest
  { id' :: Id,
    summary :: Maybe T.Text,
    estimate :: Maybe Int64,
    laneId :: Maybe Id,
    priorityId :: Maybe Id
  }
  deriving (Eq, Show, Generic)

instance FromJSON UpdateTaskRequest

instance ToRow UpdateTaskRequest where
  toRow (UpdateTaskRequest id' c1 c2 c3 c4) =
    toRow
      ( id',
        c1,
        c2,
        c3,
        c4
      )

data SearchTaskRequest = SearchTaskRequest {}
  deriving (Eq, Show, Generic)

instance FromJSON SearchTaskRequest

class TaskStore m where
  getTask :: Id -> m TaskView
  addTask :: AddTaskRequest -> m TaskView
  updateTask :: UpdateTaskRequest -> m TaskView

class TaskSearch m where
  searchTask :: SearchTaskRequest -> m [TaskView]

-- Todo

data Todo = Todo
  { id' :: Id,
    date :: UTCTime,
    note :: T.Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

instance FromRow Todo where
  fromRow =
    Todo
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field

-- Timer

data Timer = Timer
  { id' :: Id,
    label :: T.Text,
    timerTypeId :: Id,
    startedAt :: UTCTime,
    duration :: Int64
  }

instance FromRow Timer where
  fromRow =
    Timer
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field

data Pomodoro = Pomodoro
  { id' :: Id,
    taskId :: Id,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

data Break = Break
  { id' :: Id,
    taskId :: Id,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
