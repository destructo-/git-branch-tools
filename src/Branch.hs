{-# LANGUAGE OverloadedStrings #-}

module Branch
( toBranchList
, branchName
, branchLabel
, filterLocal
, filterRemote
, findCurrent
, Branch(..)
) where

import Data.Text (Text)
import Data.Char (isSpace)
import Data.List (find)
import qualified Data.Text as T

data Branch = GitLocal Text | GitCurrent Text | GitRemote Text Text
  deriving (Ord, Eq)


toBranchList :: Text -> [Branch]
toBranchList input = toBranch <$> filter validBranch (T.lines input)
 where
  validBranch b = not $ isHead b || isDetachedHead b || isNoBranch b


filterLocal :: [Branch] -> [Branch]
filterLocal = filter isLocal
  where
    isLocal (GitLocal _) = True
    isLocal (GitCurrent _) = True
    isLocal _ = False


filterRemote :: [Branch] -> [Branch]
filterRemote = filter isRemote
  where
    isRemote (GitRemote _ _) = True
    isRemote _ = False


findCurrent :: [Branch] -> Maybe Branch
findCurrent = find isCurrent
  where
    isCurrent (GitCurrent _) = True
    isCurrent _ = False


branchLabel :: Branch -> Text
branchLabel (GitCurrent n) = n
branchLabel (GitLocal n) = n
branchLabel (GitRemote _ n) = n


branchName :: Branch -> Text
branchName (GitCurrent n) = n
branchName (GitLocal n) = n
branchName (GitRemote r n) = r <> "/" <> n


toBranch :: Text -> Branch
toBranch line = mkBranch $ T.words $ T.dropWhile isSpace line
 where
  mkBranch ("*" : name : _) = GitCurrent name
  mkBranch (name : _) = case T.stripPrefix "remotes/" name of
    Just rest -> parseRemoteBranch rest
    Nothing -> GitLocal name
  mkBranch [] = error "empty branch name"
  parseRemoteBranch str = GitRemote remote name
   where
    (remote, rest) = T.span ('/' /=) str
    name = T.drop 1 rest


isHead :: Text -> Bool
isHead = T.isInfixOf "HEAD"


isDetachedHead :: Text -> Bool
isDetachedHead = T.isInfixOf "HEAD detached"


-- While rebasing git will show "no branch"
-- e.g. "* (no branch, rebasing branch-name)"
isNoBranch :: Text -> Bool
isNoBranch = T.isInfixOf "(no branch,"
