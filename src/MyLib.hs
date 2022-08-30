module MyLib (someFunc) where

import Ilyaletre.Ly.Database (run)

someFunc :: IO ()
someFunc = do
  putStrLn "Hello"
  putStrLn "World"
  run
