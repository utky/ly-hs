module Main where

import Database.SQLite.Simple (withConnection)
import Ilyaletre.Ly.Database (initializeDB)

main :: IO ()
main = do
  withConnection ":memory:" initializeDB
