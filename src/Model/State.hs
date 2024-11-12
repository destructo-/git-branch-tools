{-# LANGUAGE TemplateHaskell #-}
module Model.State where

import Model.Branch 
import Control.Lens (makeLenses)
import Data.List (find)
import Control.Lens.Operators
import Model.WidgetName
import Model.BranchList

data State = State 
  { _currentBranch    :: Maybe Branch
  , _localBranchList  :: BranchList
  , _remoteBranchList :: BranchList
  , _focus            :: WidgetName
  , _processMessage   :: Maybe String
  }

makeLenses ''State

emptyState :: State 
emptyState = State 
  { _currentBranch    = Nothing 
  , _localBranchList  = branchListEmpty Local
  , _remoteBranchList = branchListEmpty Remote
  , _focus            = Local
  , _processMessage   = Nothing
  }

setBranchList :: State -> [Branch] -> State
setBranchList state newBranchList = 
  state & localBranchList  .~ makeBranchList local Local
        & remoteBranchList .~ makeBranchList remote Remote
        & currentBranch    .~ current
  where
    current = find isCurrent newBranchList
    local   = filter isLocal newBranchList
    remote  = filter isRemote newBranchList

updateLocalBranchList :: State -> [Branch] -> State
updateLocalBranchList state newBranchList =
  state & currentBranch   .~ current
        & localBranchList .~ localList'
  where
    current    = find isCurrent newBranchList
    local      = filter isLocal newBranchList
    localList  = makeBranchList local Local
    localList' = maybe localList (`selectBranchListElement` localList) current

updateRemoteBranchList :: State -> [Branch] -> State
updateRemoteBranchList state newBranchList =
  state & remoteBranchList .~ remoteList'
  where
    selected    = getSelectedElement $ state ^. remoteBranchList
    remote      = filter isRemote newBranchList
    remoteList  = makeBranchList remote Remote
    remoteList' = maybe remoteList (`selectBranchListElement` remoteList) selected
