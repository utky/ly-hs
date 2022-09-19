module Test.Ilyaletre.Ly.Database (tests) where

import Database.SQLite.Simple (withConnection)
import Ilyaletre.Ly.Database (initializeDB)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Database"
    [ testCase "initialize" $ do
        withConnection ":memory:" initializeDB
        assertEqual "dummy" True True
    ]
