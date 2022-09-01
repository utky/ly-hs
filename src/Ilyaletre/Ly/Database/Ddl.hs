{-# LANGUAGE QuasiQuotes #-}

module Ilyaletre.Ly.Database.Ddl (ddl) where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

ddl :: [Query]
ddl =
  [ lane,
    priority,
    task,
    todo,
    todoTask,
    timerType,
    timer,
    timerTask,
    estimate,
    pomodoro,
    break',
    interruption,
    tag,
    taggedTask
  ]

lane :: Query
lane =
  [sql|
CREATE TABLE "lane"
  (
     "id"         INTEGER PRIMARY KEY,
     "name"       VARCHAR NOT NULL,
     "created_at" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     "update_at"  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
  );
|]

priority :: Query
priority =
  [sql|
CREATE TABLE "priority"
  (
     "id"         INTEGER PRIMARY KEY,
     "name"       VARCHAR NOT NULL,
     "created_at" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     "update_at"  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
  );
|]

task :: Query
task =
  [sql|
CREATE TABLE "task"
  (
     "id"          INTEGER PRIMARY KEY,
     "summary"     VARCHAR NOT NULL,
     "estimate"    INTEGER NOT NULL,
     "lane_id"     INTEGER NOT NULL REFERENCES "lane" ON DELETE RESTRICT ON UPDATE RESTRICT,
     "priority_id" INTEGER NOT NULL REFERENCES "priority" ON DELETE RESTRICT ON UPDATE RESTRICT,
     "created_at"  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     "update_at"   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
  );
|]

todo :: Query
todo =
  [sql|
CREATE TABLE "todo"
  (
     "id"         INTEGER PRIMARY KEY,
     "date"       DATE NOT NULL,
     "note"       VARCHAR NULL,
     "created_at" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     "update_at"  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     CONSTRAINT "unique_date" UNIQUE ("date")
  );
|]

todoTask :: Query
todoTask =
  [sql|
CREATE TABLE "todo_task"
  (
     "id"         INTEGER PRIMARY KEY,
     "todo_order" INTEGER NOT NULL,
     "todo_id"    INTEGER NOT NULL REFERENCES "todo" ON DELETE RESTRICT ON UPDATE RESTRICT,
     "task_id"    INTEGER NOT NULL REFERENCES "task" ON DELETE RESTRICT ON UPDATE RESTRICT,
     "created_at" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     "update_at"  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
  );
|]

timerType :: Query
timerType =
  [sql|
CREATE TABLE "timer_type"
  (
     "id"         INTEGER PRIMARY KEY,
     "name"       VARCHAR NOT NULL,
     "created_at" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     "update_at"  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
  );
|]

timer :: Query
timer =
  [sql|
CREATE TABLE "timer"
  (
     "id"            INTEGER PRIMARY KEY,
     "label"         VARCHAR NOT NULL,
     "timer_type_id" INTEGER NOT NULL REFERENCES "timer_type" ON DELETE RESTRICT ON UPDATE RESTRICT,
     "started_at"    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     "duration"      INTEGER NOT NULL
  );
|]

timerTask :: Query
timerTask =
  [sql|
CREATE TABLE "timer_task"
  (
     "id"         INTEGER PRIMARY KEY,
     "timer_id"   INTEGER NOT NULL REFERENCES "timer" ON DELETE RESTRICT ON UPDATE RESTRICT,
     "task_id"    INTEGER NOT NULL REFERENCES "task" ON DELETE RESTRICT ON UPDATE RESTRICT,
     "created_at" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     "update_at"  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     CONSTRAINT "unique_timer_task" UNIQUE ("timer_id", "task_id")
  );
|]

estimate :: Query
estimate =
  [sql|
CREATE TABLE "estimate"
  (
     "id"         INTEGER PRIMARY KEY,
     "value"      INTEGER NOT NULL,
     "task_id"    INTEGER NOT NULL REFERENCES "task" ON DELETE RESTRICT ON UPDATE RESTRICT,
     "created_at" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     "update_at"  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
  );
|]

pomodoro :: Query
pomodoro =
  [sql|
CREATE TABLE "pomodoro"
  (
     "id"         INTEGER PRIMARY KEY,
     "task_id"    INTEGER NOT NULL REFERENCES "task" ON DELETE RESTRICT ON UPDATE RESTRICT,
     "created_at" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     "update_at"  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
  );
|]

break' :: Query
break' =
  [sql|
CREATE TABLE "break"
  (
     "id"         INTEGER PRIMARY KEY,
     "created_at" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     "update_at"  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
  );
|]

interruption :: Query
interruption =
  [sql|
CREATE TABLE "interruption"
  (
     "id"         INTEGER PRIMARY KEY,
     "external"   BOOLEAN NOT NULL,
     "task_id"    INTEGER NOT NULL REFERENCES "task" ON DELETE RESTRICT ON UPDATE RESTRICT,
     "created_at" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     "update_at"  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
  );
|]

tag :: Query
tag =
  [sql|
CREATE TABLE "tag"
  (
     "id"         INTEGER PRIMARY KEY,
     "name"       VARCHAR NOT NULL,
     "created_at" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     "update_at"  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
  );
|]

taggedTask :: Query
taggedTask =
  [sql|
CREATE TABLE "tagged_task"
  (
     "id"         INTEGER PRIMARY KEY,
     "tag_id"     INTEGER NOT NULL REFERENCES "tag" ON DELETE RESTRICT ON UPDATE RESTRICT,
     "task_id"    INTEGER NOT NULL REFERENCES "task" ON DELETE RESTRICT ON UPDATE RESTRICT,
     "created_at" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     "update_at"  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     CONSTRAINT "unique_tagged_task" UNIQUE ("tag_id", "task_id")
  ); 
|]
