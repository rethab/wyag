module Main where

import Wyag.Init as Init
import Cmd

main :: IO ()
main = do
  command <- parseCommand
  case command of
    Init path -> Init.run path
    Hash_Object -> putStrLn "hash-object: not implemented"
    Cat_File -> putStrLn "cat-file: not implemented"
