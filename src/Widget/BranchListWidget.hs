module Widget.BranchListWidget
( drawBranchList
) where

import qualified Brick.Widgets.List as L

import Model.Branch
import Brick
import Brick.Widgets.Border
import Model.WidgetName
import Model.BranchList

type IsFocused = Bool

drawBranchList :: BranchList -> IsFocused -> Widget WidgetName
drawBranchList list isFocused = 
  padLeftRight 1 $
  vBox
    [ drawListLabel list
    , L.renderList drawBranchListElement isFocused list
    ]

drawBranchListElement :: Bool -> Branch -> Widget WidgetName
drawBranchListElement _ branch = padRight Max $ str $ show branch 

drawListLabel :: BranchList -> Widget WidgetName
drawListLabel list = hBorderWithLabel $ padLeftRight 1 $ str $ show $ L.listName list

