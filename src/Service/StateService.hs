module Service.StateService
( StateService(..)
, stateService 
) where

import qualified Data.Text as T

import Repository.GitRepository
import Model.State
import Control.Lens.Operators
import Model.WidgetName
import Model.BranchList
import Model.Action

data StateService = StateService
  { initState   :: IO State
  , checkOut    :: State -> IO State
  , setFocus    :: WidgetName -> State -> State
  , fetch       :: State -> IO State
  , deleteLocal :: State -> IO State
  , setAction   :: Action -> State -> State
  }

stateService :: StateService
stateService = StateService
  { initState   = _initState
  , checkOut    = _checkOut
  , setFocus    = _setFocus
  , fetch       = _fetch
  , deleteLocal = _deleteLocal
  , setAction   = _setAction
  }

_initState :: IO State
_initState = do
  loadedBranchList <- getBranchList gitRepo
  pure $ setBranchList emptyState loadedBranchList

_checkOut :: State -> IO State
_checkOut state = do
  _ <- maybe (pure $ T.pack "failed to checkout destinaiton is empty") (gitCheckout gitRepo) selected
  loadedBranchList <- getBranchList gitRepo
  let state' = updateLocalBranchList state loadedBranchList
  pure state'
  where 
    focused = state ^. focus
    maybeList = case focused of 
      Local  -> Just $ state ^. localBranchList
      Remote -> Just $ state ^. remoteBranchList
    selected = maybeList >>= getSelectedElement

_deleteLocal :: State -> IO State
_deleteLocal state = do
  _ <- maybe (pure $ T.pack "failed to delete branch") (gitDeleteBranch gitRepo) selected
  loadedBranchList <- getBranchList gitRepo
  let state' = updateLocalBranchList state loadedBranchList
  pure state'
  where
    focused = state ^. focus
    maybeList = case focused of 
      Local -> Just $ state ^. localBranchList
      _     -> Nothing
    selected = maybeList >>= getSelectedElement

_fetch :: State -> IO State
_fetch state = do
  _ <- gitFetch gitRepo
  loadedBranchList <- getBranchList gitRepo
  let state' = updateRemoteBranchList state loadedBranchList
  pure state'

_setFocus :: WidgetName -> State -> State
_setFocus widgetName = focus .~ widgetName

_setAction :: Action -> State -> State
_setAction newAction = action .~ newAction
