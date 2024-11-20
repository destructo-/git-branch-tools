{-# LANGUAGE OverloadedStrings #-}
module Model.Branch where

import qualified Data.Text as T

import Domain.HasTextName
import Control.Monad (mfilter)
import Control.Applicative

type Name   = T.Text
type Origin = T.Text

data Branch
  = BranchLocal Name
  | BranchCurrent Name
  | BranchRemote Origin Name
  deriving (Ord, Eq)

instance Show Branch where
  show (BranchLocal name)         = T.unpack $ "  " <> name
  show (BranchCurrent name)       = T.unpack $ "* " <> name
  show (BranchRemote origin name) = T.unpack $ origin <> "/" <> name

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

makeBranch :: T.Text -> Maybe Branch
makeBranch text =
  mfilter isBranch (remote <|> local <|> current) >>= \t -> case T.words t of
    ("*" : name : _)          -> Just $ BranchCurrent name
    ("l" : name : _)          -> Just $ BranchLocal name
    ("r" : origin : name : _) -> Just $ BranchRemote origin name
    _                         -> Nothing
  where
    isHead         = T.isInfixOf "HEAD"
    isDetachedHead = T.isInfixOf "HEAD detached"
    isNoBranch     = T.isInfixOf "(no branch,"
    isBranch t     = not $ isHead t || isDetachedHead t || isNoBranch t
    local          = T.append "l " <$> T.stripPrefix "refs/heads/" (T.stripStart text)
    current        = T.append "* " <$> (T.stripPrefix "* " text >>= T.stripPrefix "refs/heads/")
    remote         = T.append "r " 
      <$> (\(x,y) -> x <> " " <> T.drop 1 y) 
      <$> T.span ('/' /=)
      <$> T.stripPrefix "refs/remotes/" (T.stripStart text)

