{-# LANGUAGE OverloadedStrings #-}
module Wyag.Repo (
    Repo(..)
  , init
  , create
  , find
) where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Ini
import qualified Data.HashMap.Strict as M
import           System.FilePath
import           System.Directory
import           Data.Text.Read
import           Prelude hiding (init)

data Repo = Repo {
    worktree :: FilePath
  , gitdir :: FilePath
  , conf :: Ini
}

init :: FilePath -> IO (Either String Repo)
init worktree = do
  let gitdir = worktree </> ".git"
  isdir <- doesDirectoryExist gitdir
  if not isdir then return (Left $ "Not a git repository: " ++ worktree)
  else do
    cfg <- readIniFile (gitdir </> "config")
    case cfg of
      Left err -> return (Left $ "Failed to read config file: " ++ err)
      Right ini ->
        case parseRepoVersion ini of
           Left err -> return (Left $ "Failed to read 'core.repositoryformatversion': " ++ err)
           Right 0  -> return (Right $ Repo worktree gitdir ini)
           Right v  -> return (Left $ "Unsupported repositoryversionformat: " ++ show v)

parseRepoVersion :: Ini -> Either String Int
parseRepoVersion ini = 
  do strVersion <- lookupValue "core" "repositoryformatversion" ini
     fst <$> decimal strVersion

create :: FilePath -> IO (Either String Repo)
create path = runExceptT $ do
  workdir <- ExceptT $ createDir path
  let gitdir = workdir </> ".git"
  _ <- traverse (ExceptT . createRepoDir gitdir) ["branches", "objects", "refs/tags", "refs/heads"]
  _ <- liftIO $ (const unused) <$> writeFile (gitdir </> "HEAD") "ref: refs/heads/master\n"
  _ <- liftIO $ (const unused) <$> writeIniFile (gitdir </> "config") defaultConfig
  ExceptT $ init path

  where
    unused :: Either String ()
    unused = Right ()

    defaultConfig :: Ini
    defaultConfig = Ini (M.fromList [("core", elems)])
      where
        elems = M.fromList
          [ ("repositoryformatversion", "0")
          , ("filemode", "false")
          , ("bare", "false")
          ]


    createDir :: FilePath -> IO (Either String FilePath)
    createDir p = do
      exists <- doesPathExist p
      if not exists then const (Right p) <$> createDirectory p
      else do
        isdir <- doesDirectoryExist path
        if not isdir then return (Left $ "Not a directory: " ++ path)
        else do
          contents <- listDirectory path
          if not (null contents) then return (Left $ "Directory is not empty: " ++ path)
          else return (Right p)

createRepoDir :: FilePath -> String -> IO (Either String FilePath)
createRepoDir repo dir = do
  exists <- doesPathExist (repo </> dir)
  if exists then do 
    isdir <- doesDirectoryExist (repo </> dir)
    if isdir then return (Right $ repo </> dir)
    else return (Left $ "Not a directory: " ++ dir)
  else do
    const (Right $ repo </> dir) <$> createDirectoryIfMissing True (repo </> dir)

find :: IO (Either String Repo)
find = getCurrentDirectory >>= go
  where
    go :: FilePath -> IO (Either String Repo)
    go path = doesDirectoryExist (path </> ".git") >>= \isrepo ->
                if isrepo then init path
                else if path == "/" then return (Left "No repository here")
                     else makeAbsolute (path </> "..") >>= go
