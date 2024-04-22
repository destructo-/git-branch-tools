module State
( State(..)
, Focus(..)
, selectRigth
, selectLeft
, selectNext
, selectPrevious
, getInitialState
, checkout
, fetch
) where

import Branch (filterLocal, filterRemote, findCurrent, Branch, branchName)
import Commands

data Focus = LeftFocus | RightFocus
  deriving (Ord, Eq)

data State = State
  { currentBranch :: Maybe Branch
  , localBranchList :: [(Int, Branch)]
  , remoteBranchList :: [(Int, Branch)]
  , selectedLeft :: (Int, Branch)
  , selectedRigth :: (Int, Branch)
  , focus :: Focus
  } deriving(Ord, Eq)


selectRigth :: State -> State
selectRigth state =
  case focus state of
    RightFocus -> state
    LeftFocus -> state { focus = RightFocus }


selectLeft :: State -> State
selectLeft state =
  case focus state of
    LeftFocus -> state
    RightFocus -> state { focus = LeftFocus }


selectNext :: State -> State
selectNext state = case focus state of
  LeftFocus ->
    if currentLeftIndex >= (length (localBranchList state) - 1) then state
    else state { selectedLeft = localBranchList state !! (currentLeftIndex + 1) }
  RightFocus ->
    if currentRigthIndex >= (length (remoteBranchList state) - 1) then state
    else state { selectedRigth = remoteBranchList state !! (currentRigthIndex + 1) }
  where
    currentLeftIndex = fst $ selectedLeft state
    currentRigthIndex = fst $ selectedRigth state


selectPrevious :: State -> State
selectPrevious state = case focus state of
  LeftFocus ->
    if currentLeftIndex <= 0 then state
    else state { selectedLeft = localBranchList state !! (currentLeftIndex - 1) }
  RightFocus ->
    if currentRigthIndex <= 0 then state
    else state { selectedRigth = remoteBranchList state !! (currentRigthIndex - 1) }
  where
    currentLeftIndex = fst $ selectedLeft state
    currentRigthIndex = fst $ selectedRigth state


checkout :: State -> IO State
checkout state = case focus state of
  LeftFocus -> do
    _ <- gitCheckout $ snd $ selectedLeft state
    reloadBranches state
  RightFocus -> do
    _ <- gitCheckout $ snd $ selectedRigth state
    reloadBranches state


fetch :: State -> IO State
fetch state = do
  _ <- gitFetch
  reloadBranches state


getInitialState :: IO State
getInitialState = do
  loadedList <- getBranchList
  let current = findCurrent loadedList
  let local = zip [0..] $ filterLocal loadedList
  let remote = zip [0..] $ filterRemote loadedList
  pure $ State
    { currentBranch = current
    , localBranchList = local
    , remoteBranchList = remote
    , selectedLeft = head local -- TODO if local is empty application application failed
    , selectedRigth = head remote
    , focus = LeftFocus
    }


reloadBranches :: State -> IO State
reloadBranches state = do
  loadedList <- getBranchList
  let current = findCurrent loadedList
  let local = zip [0..] $ filterLocal loadedList
  let remote = zip [0..] $ filterRemote loadedList
  pure $ state
    { currentBranch = current
    , localBranchList = local
    , remoteBranchList = remote
    , selectedLeft = updateSelection local $ selectedLeft state
    , selectedRigth = updateSelection remote $ selectedRigth state
    }


updateSelection :: [(Int, Branch)] -> (Int, Branch) -> (Int, Branch)
updateSelection list selected = head $ filter (\x -> branchName (snd x) == branchName (snd selected)) list
