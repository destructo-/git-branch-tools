{-# LANGUAGE OverloadedStrings #-}

module Commands
( isGitRepository
, getBranchList
, gitCheckout
, gitFetch
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


branchLog :: Branch -> [Text]
branchLog branch = 
  [ "branch"
  , "log"
  , "--no-color"
  , branchLabel branch
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


gitCheckout :: Branch -> IO Text
gitCheckout branch = readGitWithExitCode ["checkout", branchLabel branch]


gitFetch :: IO Text
gitFetch = do 
  message <- readGitWithExitCode ["fetch", "-p"]
  pure $ orDefault message "fetching done"


readGit :: [Text] -> IO Text
readGit args = T.pack <$> readProcess "git" (T.unpack <$> args) []


readGitWithExitCode :: [Text] -> IO Text
readGitWithExitCode args = do
  resp <- readProcessWithExitCode "git" (T.unpack <$> args) []
  let str = getSecond resp
  pure $ T.pack str


spawnGit :: [Text] -> IO ExitCode
spawnGit args = waitForProcess =<< spawnProcess "git" (T.unpack <$> args)

-- TODO handle errors on process
getSecond :: (a, b, c) -> b
getSecond (_, b, _) = b


orDefault :: Text -> String -> Text
orDefault text fallBack = 
  if T.null text
    then T.pack fallBack
    else text
