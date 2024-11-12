{-# LANGUAGE OverloadedStrings #-}
module Model.Branch where

import qualified Data.Text as T

import Data.Text
import Data.Char (isSpace)
import Domain.HasTextName

type Name   = T.Text
type Origin = T.Text

data Branch
  = BranchLocal Name 
  | BranchCurrent Name 
  | BranchRemote Origin Name
  deriving (Ord, Eq)

instance Show Branch where
  show (BranchLocal name)         = unpack $ "  " <> name
  show (BranchCurrent name)       = unpack $ "* " <> name
  show (BranchRemote origin name) = unpack $ origin <> "/" <> name

instance HasTextName Branch where
  getName (BranchLocal    name) = name
  getName (BranchCurrent  name) = name
  getName (BranchRemote _ name) = name
  
isLocal :: Branch -> Bool
isLocal (BranchLocal _)   = True
isLocal (BranchCurrent _) = True
isLocal _                 = False

isRemote :: Branch -> Bool
isRemote (BranchRemote _ _) = True
isRemote _                  = False

isCurrent :: Branch -> Bool
isCurrent (BranchCurrent _) = True
isCurrent _                 = False

makeBranch :: T.Text -> Branch
makeBranch line = mkBranch $ T.words $ T.dropWhile isSpace line
  where
    mkBranch ("*" : name : _) = BranchCurrent name
    mkBranch (name : _) = case T.stripPrefix "remotes/" name of
      Just rest -> parseRemoteBranch rest
      Nothing -> BranchLocal name
    mkBranch [] = error "empty branch name"
    parseRemoteBranch str = BranchRemote remote name
      where
        (remote, rest) = T.span ('/' /=) str
        name = T.drop 1 rest
