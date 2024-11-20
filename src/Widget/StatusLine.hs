{-# LANGUAGE OverloadedStrings #-}
module Widget.StatusLine
( drawStatusLine
) where

import qualified Brick.Widgets.Center as C

import Brick
import Model.Branch
import Config.Config
import Domain.HasTextName
import Model.WidgetName
import Model.Action

drawStatusLine :: Action -> Maybe Branch -> Widget WidgetName
drawStatusLine event branch = 
  padLeftRight 1 $ 
  hBox 
    [ (C.hCenter $ padTopBottom 1 $ withAttr attrStatsLine $ vLimit 1 $ str $ branchNotification branch)
    <=> (C.hCenter $ padTopBottom 1 $ withAttr attrStatsLine $ vLimit 1 $ str $ eventNotification event)
    ]

branchNotification :: Maybe Branch -> String
branchNotification Nothing = "failed to detect current branch"
branchNotification (Just branch) = "now on [" ++ showName branch ++ "] branch"

eventNotification :: Action -> String
eventNotification ev = show ev
