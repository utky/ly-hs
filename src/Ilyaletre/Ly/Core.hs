{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Ilyaletre.Ly.Core
  ( Id,
    Lane (Lane),
    HasLane (..),
    Priority (Priority),
    HasPriority (..),
    Task (Task),
    HasTask (..),
    TaskView (TaskView),
    HasTaskView (..),
    AddTaskRequest (AddTaskRequest),
    HasAddTaskRequest (..),
    UpdateTaskRequest (UpdateTaskRequest),
    HasUpdateTaskRequest (..),
    TaskStore (..),
  )
where

import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import Database.SQLite.Simple.ToRow (ToRow (..))
import Lens.Micro.TH (makeClassy)

-- Type

type Id = Int64

-- Lane

data Lane = Lane
  { _laneId :: Id,
    _laneName :: T.Text,
    _laneCreatedAt :: UTCTime,
    _laneUpdatedAt :: UTCTime
  }
  deriving (Eq, Show)

makeClassy ''Lane

instance FromRow Lane where
  fromRow =
    Lane
      <$> field
      <*> field
      <*> field
      <*> field

-- Priority

data Priority = Priority
  { _priorityId :: Id,
    _priorityName :: T.Text,
    _priorityCreatedAt :: UTCTime,
    _priorityUpdatedAt :: UTCTime
  }
  deriving (Eq, Show)

makeClassy ''Priority

instance FromRow Priority where
  fromRow =
    Priority
      <$> field
      <*> field
      <*> field
      <*> field

-- Task

data Task = Task
  { _taskId :: Id,
    _taskSummary :: T.Text,
    _taskEstimate :: Int64,
    _taskLaneId :: Id,
    _taskPriorityId :: Id,
    _taskCreatedAt :: UTCTime,
    _taskUpdatedAt :: UTCTime
  }
  deriving (Eq, Show)

makeClassy ''Task

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
  { _taskViewId :: Id,
    _taskViewSummary :: T.Text,
    _taskViewEstimate :: Int64,
    _taskViewLaneId :: Id,
    _taskViewLane :: T.Text,
    _taskViewPriorityId :: Id,
    _taskViewPriority :: T.Text,
    _taskViewCreatedAt :: UTCTime,
    _taskViewUpdatedAt :: UTCTime
  }
  deriving (Eq, Show)

makeClassy ''TaskView

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
  { _addTaskRequestSummary :: T.Text,
    _addTaskRequestEstimate :: Int64,
    _addTaskRequestLaneId :: Id,
    _addTaskRequestPriorityId :: Id
  }

makeClassy ''AddTaskRequest

instance ToRow AddTaskRequest where
  toRow (AddTaskRequest c1 c2 c3 c4) =
    toRow
      ( c1,
        c2,
        c3,
        c4
      )

data UpdateTaskRequest = UpdateTaskRequest
  { _updateTaskRequestId :: Id,
    _updateTaskRequestSummary :: Maybe T.Text,
    _updateTaskRequestEstimate :: Maybe Int64,
    _updateTaskRequestLaneId :: Maybe Id,
    _updateTaskRequestPriorityId :: Maybe Id
  }

makeClassy ''UpdateTaskRequest

instance ToRow UpdateTaskRequest where
  toRow (UpdateTaskRequest id' c1 c2 c3 c4) =
    toRow
      ( id',
        c1,
        c2,
        c3,
        c4
      )

class TaskStore m where
  getTask :: Id -> m TaskView
  addTask :: AddTaskRequest -> m TaskView
  updateTask :: UpdateTaskRequest -> m TaskView

-- Todo

data Todo = Todo
  { _todoId :: Id,
    _todoDate :: UTCTime,
    _todoNote :: T.Text,
    _todoCreatedAt :: UTCTime,
    _todoUpdatedAt :: UTCTime
  }

makeClassy ''Todo

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
  { _timerId :: Id,
    _timerLabel :: T.Text,
    _timerTimerTypeId :: Id,
    _timerStartedAt :: UTCTime,
    _timerDuration :: Int64
  }

makeClassy ''Timer

instance FromRow Timer where
  fromRow =
    Timer
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
