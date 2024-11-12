module Widget.StatusLine
( drawStatusLine
) where

import qualified Brick.Widgets.Center as C

import Brick
import Model.Branch
import Config.Config
import Domain.HasTextName
import Model.WidgetName

drawStatusLine :: Maybe Branch -> Widget WidgetName
drawStatusLine branch = 
  padLeftRight 1 $ 
  hBox 
    [C.hCenter $ padTopBottom 1 $ withAttr attrStatsLine $ vLimit 1 $ str $ branchNotification branch ]

branchNotification :: Maybe Branch -> String
branchNotification Nothing = "failed to detect current branch"
branchNotification (Just branch) = "now on [" ++ showName branch ++ "] branch"
