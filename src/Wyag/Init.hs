module Wyag.Init
    ( run
    ) where

import Wyag.Repo

run :: FilePath -> IO ()
run path = do
  res <- create path
  case res of 
    Left err -> putStrLn err
    Right _ -> putStrLn "Initialized Repository"
