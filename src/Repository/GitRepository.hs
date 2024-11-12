{-# LANGUAGE OverloadedStrings #-}
module Repository.GitRepository
( GitRepository(..) 
, gitRepo
) where

import qualified Data.Text as T
import qualified Data.List as L

import System.Process
import System.Directory
import Model.Branch
import Domain.HasTextName

data GitRepository = GitRepository
  { isGitDirectory :: FilePath -> IO Bool 
  , getBranchList  :: IO [Branch]
  , gitCheckout :: Branch -> IO T.Text
  , gitDeleteBranch :: Branch -> IO T.Text
  , gitFetch :: IO T.Text
  }

gitRepo :: GitRepository
gitRepo = GitRepository 
  { isGitDirectory  = _isGitRepository
  , getBranchList   = _getBranchList
  , gitCheckout     = _gitCheckout
  , gitDeleteBranch = _gitDeleteBranch
  , gitFetch        = _gitFetch
  }

_isGitRepository :: FilePath -> IO Bool
_isGitRepository path = do
  contentList <- getDirectoryContents path
  let element = L.find (== ".git") contentList
  case element of
    Just _  -> pure True
    Nothing -> pure False

_getBranchList :: IO [Branch]
_getBranchList = toBranchList <$> readGit branchesArgs

_gitCheckout :: Branch -> IO T.Text
_gitCheckout branch = 
  readGitWithExitCode ["checkout", getName branch]

_gitDeleteBranch :: Branch -> IO T.Text
_gitDeleteBranch branch =
  readGitWithExitCode ["branch", "-D", getName branch]

_gitFetch :: IO T.Text
_gitFetch =
  readGitWithExitCode ["fetch", "-p"]

-- console commands arguments
branchesArgs :: [T.Text]
branchesArgs =
  [ "branch"
  , "--list"
  , "--all"
  , "--sort=-committerdate"
  , "--no-column"
  , "--no-color"
  ]

-- read console response utils
readGit :: [T.Text] -> IO T.Text
readGit args = T.pack <$> readProcess "git" (T.unpack <$> args) []

readGitWithExitCode :: [T.Text] -> IO T.Text
readGitWithExitCode args = do
  resp <- readProcessWithExitCode "git" (T.unpack <$> args) []
  let (_, str, _) = resp
  pure $ T.pack str

-- console response => Branch utils
toBranchList :: T.Text -> [Branch]
toBranchList input = makeBranch <$> filter validBranch (T.lines input)
  where
    validBranch b = not $ isHead b || isDetachedHead b || isNoBranch b

isHead :: T.Text -> Bool
isHead = T.isInfixOf "HEAD"

isDetachedHead :: T.Text -> Bool
isDetachedHead = T.isInfixOf "HEAD detached"

-- While rebasing git will show "no branch"
-- e.g. "* (no branch, rebasing branch-name)"
isNoBranch :: T.Text -> Bool
isNoBranch = T.isInfixOf "(no branch,"
