{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Ilyaletre.Ly.Database (tests) where

import Database.SQLite.Simple (Connection, withConnection)
import Ilyaletre.Ly.Core
import Ilyaletre.Ly.Database (initializeDB, runDatabase)
import Test.Tasty
import Test.Tasty.HUnit

withTestDB :: (Connection -> IO a) -> IO a
withTestDB f = withConnection ":memory:" (\c -> initializeDB c >> f c)

tests :: TestTree
tests =
  testGroup
    "Database"
    [ testCase "initialize" testInitialize,
      testCase "add task" testAddTask,
      testCase "add task multiple" testAddTaskMultiple
    ]

testInitialize :: Assertion
testInitialize = do
  withTestDB $ const (return ())
  assertBool "initializeDB" True

testAddTask :: Assertion
testAddTask = do
  TaskView {summary, estimate, lane, priority} <- withTestDB $ \c -> do
    flip runDatabase c $
      addTask
        ( AddTaskRequest
            "test"
            1 -- estimate
            2 -- lane_id
            3 -- priority_id
        )
  summary @?= "test"
  estimate @?= 1
  lane @?= "todo"
  priority @?= "h"

testAddTaskMultiple :: Assertion
testAddTaskMultiple = do
  (TaskView {summary = summary1, estimate = estimate1, lane = lane1, priority = priority1}, TaskView {summary = summary2, estimate = estimate2, lane = lane2, priority = priority2}) <- withTestDB $ \c -> do
    flip runDatabase c $ do
      t1 <-
        addTask
          ( AddTaskRequest
              "test1"
              1 -- estimate
              2 -- lane_id
              3 -- priority_id
          )
      t2 <-
        addTask
          ( AddTaskRequest
              "test2"
              2 -- estimate
              3 -- lane_id
              1 -- priority_id
          )
      return (t1, t2)
  summary1 @?= "test1"
  estimate1 @?= 1
  lane1 @?= "todo"
  priority1 @?= "h"

  summary2 @?= "test2"
  estimate2 @?= 2
  lane2 @?= "done"
  priority2 @?= "l"
