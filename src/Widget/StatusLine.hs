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
drawStatusLine action branch = 
  padLeftRight 1 $ 
  hBox [ drawBranchLine branch <=> drawActionLine action ]

drawBranchLine :: Maybe Branch -> Widget WidgetName
drawBranchLine branch =
  C.hCenter $
  padTopBottom 1 $
  withAttr attrStatsLine $
  vLimit 1 $
  str notification
  where
    notification = case branch of
      Just b  -> "now on [" ++ showName b ++ "] branch"
      Nothing -> "failed to detect current branch"

drawActionLine :: Action -> Widget WidgetName
drawActionLine action =
  C.hCenter $
  padTopBottom 1 $
  withAttr attrActionLine $
  vLimit 1 $
  str $
  show action
