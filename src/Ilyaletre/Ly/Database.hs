{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ilyaletre.Ly.Database (initializeDB, Database (..)) where

import Control.Monad (forM_)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (..))
import Database.SQLite.Simple (Connection, Only (Only), execute, execute_, lastInsertRowId, query)
import Database.SQLite.Simple.QQ (sql)
import Ilyaletre.Ly.Core
  ( SaveTaskRequest (..),
    TaskStore (..),
  )
import Ilyaletre.Ly.Database.Ddl (ddl)

newtype Database m a = Database {runConnection :: ReaderT Connection m a} deriving (Functor, Applicative, Monad, MonadIO)

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
              priority.name AS prirority,
              task.created_at AS created_at,
              task.updated_at AS updated_at
            FROM task
            JOIN lane ON task.lane_id = lane.id
            JOIN priority ON task.priority_id = prirority
            WHERE task.id = ?
          |]
          (Only i)
    return $ head vs

  saveTask (SaveTaskRequest i s e l p) = do
    c <- ask
    rowId <- liftIO $ do
      case i of
        (Just i') -> execute c "UPDATE task SET summary = ?, estimate = ?, lane_id = ?, prirority_id = ? WHERE id = ?" (s, e, l, p, i') >> pure i'
        Nothing -> execute c "INSERT INTO task (summary, estimate, lane_id, prirority_id) VALUES (?, ?, ?, ?)" (s, e, l, p) >> lastInsertRowId c
    getTask rowId
