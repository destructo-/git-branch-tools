module Model.BranchList where

import qualified Brick.Widgets.List as L
import qualified Data.Vector        as Vec

import Model.WidgetName
import Model.Branch
import Config.Config

type BranchList = L.List WidgetName Branch

branchListEmpty :: WidgetName -> BranchList
branchListEmpty name = L.list name Vec.empty listElementHeigth

makeBranchList :: [Branch] -> WidgetName -> BranchList
makeBranchList branch name =
  L.list name (Vec.fromList branch) listElementHeigth

selectBranchListElement :: Branch -> BranchList -> BranchList
selectBranchListElement = L.listMoveToElement 

getSelectedElement :: BranchList -> Maybe Branch
getSelectedElement = fmap snd . L.listSelectedElement
