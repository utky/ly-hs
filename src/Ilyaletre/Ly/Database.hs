{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Ilyaletre.Ly.Database (initializeDB) where

import Control.Monad (forM_)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT)
import Database.SQLite.Simple (Connection, execute_, query_)
import Database.SQLite.Simple.QQ (sql)
import GHC.Generics (Selector)
import Ilyaletre.Ly.Core
import Ilyaletre.Ly.Database.Ddl (ddl)

type Database = ReaderT Connection

initializeDB :: Connection -> IO ()
initializeDB conn = forM_ ddl $ \q -> do
  execute_ conn q

instance (MonadIO m) => MonadTask (Database m) where
  getTask :: Id -> Database m TaskView
  getTask i = do
    c <- ask
    vs <-
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
            priority.name AS prirority,
            task.created_at AS created_at,
            task.updated_at AS updated_at
          FROM task
          JOIN lane ON task.lane_id = lane.id
          JOIN priority ON task.priority_id = prirority
          WHERE task.id = ?
          |]
    head vs

  saveTask :: TaskRequest -> Database m TaskView
  saveTask (TaskAdd s e l p) = do
    c <- ask
    _ <- liftIO $ execute c "INSERT INTO task (summary, estimate, lane_id, prirority_id) VALUES (?, ?, ?, ?)" (s, e, l, p)
    rowId <- lastInsertRowId c
    getTask rowId
  saveTask (TaskUpdate i s e l p) = do
    c <- ask
    _ <- liftIO $ execute c "UPDATE task SET summary = ?, estimate = ?, lane_id = ?, prirority_id = ? WHERE id = ?" (s, e, l, p, i)
    getTask i
