module Main where

import Lib qualified (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Lib.someFunc
