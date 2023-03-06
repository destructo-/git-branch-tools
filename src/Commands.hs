{-# LANGUAGE OverloadedStrings #-}

module Commands
( isGitRepository
, getBranchList
, gitCheckout
, gitFetch
, gitPrune
) where

import System.Process
import System.Directory (getDirectoryContents)
import Data.List (find)
import Branch (Branch, toBranchList, branchLabel)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO.Exception


branchesArgs :: [Text]
branchesArgs =
  [ "branch"
  , "--list"
  , "--all"
  , "--sort=-committerdate"
  , "--no-column"
  , "--no-color"
  ]


isGitRepository :: FilePath -> IO Bool
isGitRepository path = do
  contentList <- getDirectoryContents path
  let element = find (== ".git") contentList
  case element of
    Just _  -> pure True
    Nothing -> pure False


getBranchList :: IO [Branch]
getBranchList = toBranchList <$> readGit branchesArgs


gitCheckout :: Branch -> IO ExitCode
gitCheckout branch = spawnGit ["checkout", branchLabel branch]


gitFetch :: IO ExitCode
gitFetch = spawnGit ["fetch"]


gitPrune :: IO ExitCode
gitPrune = spawnGit ["prune"]

readGit :: [Text] -> IO Text
readGit args = T.pack <$> readProcess "git" (T.unpack <$> args) []


spawnGit :: [Text] -> IO ExitCode
spawnGit args = waitForProcess =<< spawnProcess "git" (T.unpack <$> args)