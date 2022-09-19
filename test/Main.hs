module Main (main) where

import qualified Test.Ilyaletre.Ly.Database as Database
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Test" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "UnitTest"
    [ Database.tests
    ]
