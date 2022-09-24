{-# LANGUAGE OverloadedStrings #-}

module Test.Ilyaletre.Ly.Database (tests) where

import Database.SQLite.Simple (Connection, withConnection)
import Ilyaletre.Ly.Core
import Ilyaletre.Ly.Database (initializeDB, runDatabase)
import Lens.Micro
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
  v <- withTestDB $ \c -> do
    flip runDatabase c $
      addTask
        ( AddTaskRequest
            "test"
            1 -- estimate
            2 -- lane_id
            3 -- priority_id
        )
  (v ^. taskViewSummary) @?= "test"
  (v ^. taskViewEstimate) @?= 1
  (v ^. taskViewLane) @?= "todo"
  (v ^. taskViewPriority) @?= "h"

testAddTaskMultiple :: Assertion
testAddTaskMultiple = do
  (v1, v2) <- withTestDB $ \c -> do
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
  (v1 ^. taskViewSummary) @?= "test1"
  (v1 ^. taskViewEstimate) @?= 1
  (v1 ^. taskViewLane) @?= "todo"
  (v1 ^. taskViewPriority) @?= "h"

  (v2 ^. taskViewSummary) @?= "test2"
  (v2 ^. taskViewEstimate) @?= 2
  (v2 ^. taskViewLane) @?= "done"
  (v2 ^. taskViewPriority) @?= "l"
