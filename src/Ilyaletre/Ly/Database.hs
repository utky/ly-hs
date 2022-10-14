{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ilyaletre.Ly.Database (initializeDB, Database (..), runDatabase) where

import Control.Monad (forM_)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (..))
import Database.SQLite.Simple (Connection, Only (Only), execute, execute_, lastInsertRowId, query, query_)
import Database.SQLite.Simple.QQ (sql)
import Debug.Trace (trace)
import Ilyaletre.Ly.Core
import Ilyaletre.Ly.Database.Ddl (ddl)

newtype Database m a = Database {runConnection :: ReaderT Connection m a} deriving (Functor, Applicative, Monad, MonadIO)

runDatabase :: (MonadIO m) => Database m a -> Connection -> m a
runDatabase d c =
  let r = runConnection d
   in runReaderT r c

instance (Monad m) => MonadReader Connection (Database m) where
  ask = Database $ ReaderT return

initializeDB :: Connection -> IO ()
initializeDB conn = forM_ ddl $ execute_ conn

instance (MonadIO m) => TaskStore (Database m) where
  getTask i = do
    c <- ask
    vs <-
      liftIO $
        query
          c
          [sql|
            SELECT
              task.id AS id,
              task.summary AS summary,
              task.estimate AS estimate,
              task.lane_id AS lane_id,
              lane.name AS lane,
              task.priority_id AS priority_id,
              priority.name AS priority,
              task.created_at AS created_at,
              task.updated_at AS updated_at
            FROM task
            JOIN lane ON task.lane_id = lane.id
            JOIN priority ON task.priority_id = priority.id
            WHERE task.id = ?
          |]
          (Only i)
    return $ head vs

  addTask req = do
    c <- ask
    rowId <-
      liftIO $
        execute
          c
          "INSERT INTO task (summary, estimate, lane_id, priority_id) VALUES (?, ?, ?, ?)"
          req
          >> lastInsertRowId c
    getTask (trace (show rowId) rowId)

  updateTask req@UpdateTaskRequest {id'} = do
    c <- ask
    _ <-
      liftIO $
        execute
          c
          "UPDATE task SET summary = ?, estimate = ?, lane_id = ?, priority_id = ? WHERE id = ?"
          req
    getTask id'

instance (MonadIO m) => TaskSearch (Database m) where
  searchTask req = do
    c <- ask
    liftIO $
      query_
        c
        [sql|
          SELECT
            task.id AS id,
            task.summary AS summary,
            task.estimate AS estimate,
            task.lane_id AS lane_id,
            lane.name AS lane,
            task.priority_id AS priority_id,
            priority.name AS priority,
            task.created_at AS created_at,
            task.updated_at AS updated_at
          FROM task
          JOIN lane ON task.lane_id = lane.id
          JOIN priority ON task.priority_id = priority.id
          ORDER BY priority_id DESC, created_at DESC
        |]
