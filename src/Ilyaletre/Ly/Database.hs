{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ilyaletre.Ly.Database (run) where

import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Time (Day, getCurrentTime)
import Data.Time.Clock (UTCTime)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Lane
    name String
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updateAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show
Priority
    name String
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updateAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show
Task
    summary String
    estimate Int64
    laneId LaneId
    priorityId PriorityId
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updateAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show
Todo
    date Day
    note String Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updateAt UTCTime default=CURRENT_TIMESTAMP
    UniqueDate date
    deriving Show
TodoTask
    todoOrder Int64
    todoId TodoId
    taskId TaskId
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updateAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show
TimerType
    name String
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updateAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show
Timer
    label String
    timerTypeId TimerTypeId
    startedAt UTCTime default=CURRENT_TIMESTAMP
    duration Int64
    deriving Show
TimerTask
    timerId TimerId
    taskId TaskId
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updateAt UTCTime default=CURRENT_TIMESTAMP
    UniqueTimerTask timerId taskId
    deriving Show
Estimate
    value Int64
    taskId TaskId
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updateAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show
Pomodoro
    taskId TaskId
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updateAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show
Break
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updateAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show
Interruption
    external Bool
    taskId TaskId
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updateAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show
Tag
    name String
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updateAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show
TaggedTask
    tagId TagId
    taskId TaskId
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updateAt UTCTime default=CURRENT_TIMESTAMP
    UniqueTaggedTask tagId taskId
    deriving Show
|]

run :: IO ()
run = runSqlite ":memory:" $ do
  runMigration migrateAll

  now <- liftIO getCurrentTime

  laneId <- insert $ Lane "backlog" now now
  prioId <- insert $ Priority "n" now now
  taskId <- insert $ Task "test summary" 1 laneId prioId now now
  testSummary <- get taskId

  liftIO $ print testSummary
