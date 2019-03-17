{-# LANGUAGE DeriveDataTypeable #-}
module Cmd (
    Command(..)
  , parseCommand
) where

import System.Console.CmdArgs
import Prelude hiding (init)

data Command = Init { path :: FilePath }
             | Hash_Object
             | Cat_File
             deriving (Show, Data, Typeable)

init = Init {
  path = def &= opt "." &= typFile &= help "Where to create the repository"
} &= help "Initialize new Git Repository"
hash_object = Hash_Object &= help "hash object"
cat_file = Cat_File &= help "cat file"

mode = cmdArgsMode $ modes [init,hash_object,cat_file]
  &= program "wyag"
  &= summary "Write Yourself a Git"

parseCommand :: IO Command
parseCommand = cmdArgsRun mode
