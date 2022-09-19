{-# LANGUAGE OverloadedStrings #-}

module Test.Ilyaletre.Ly.Database (tests) where

import Database.SQLite.Simple (Connection, withConnection)
import Ilyaletre.Ly.Core
import Ilyaletre.Ly.Database (Database (..), initializeDB, runDatabase)
import Lens.Micro
import Test.Tasty
import Test.Tasty.HUnit

withTestDB :: (Connection -> IO a) -> IO a
withTestDB f = withConnection ":memory:" (\c -> initializeDB c >> f c)

tests :: TestTree
tests =
  testGroup
    "Database"
    [ testCase "initialize" $ do
        withTestDB $ const (return ())
        assertBool "initializeDB" True,
      testCase "add task" $ do
        v <- withTestDB $ \c -> do
          flip runDatabase c $ addTask (AddTaskRequest "test" 1 1 1)
        (v ^. taskViewSummary) @?= "test"
    ]
