{-# LANGUAGE OverloadedStrings #-}
module Application where

import qualified Brick.BChan          as BC
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List   as L

import Brick
import Model.State
import Model.WidgetName
import Handler
import Brick.Themes
import Config.Config
import Widget.BranchListWidget
import Control.Lens.Operators
import Widget.StatusLine

application :: App State e WidgetName 
application = App 
  { appDraw         = drawMainScreen
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = eventHandler
  , appStartEvent   = return ()
  , appAttrMap      = const $ themeToAttrMap theme
  }

drawMainScreen :: State -> [Widget WidgetName]
drawMainScreen state = 
  [C.hCenter $ drawLocalBranchList state <+> drawRemoteBranchList state <=> drawStatusLine selected <=> drawHelp]
  where 
    selected = state ^. currentBranch

drawLocalBranchList :: State -> Widget WidgetName
drawLocalBranchList state = 
  drawBranchList list isFocused
  where
    list        = state ^. localBranchList
    focusWidget = state ^. focus
    isFocused   = L.listName list == focusWidget

drawRemoteBranchList :: State -> Widget WidgetName
drawRemoteBranchList state =
  drawBranchList list isFocused
  where
    list        = state ^. remoteBranchList
    focusWidget = state ^. focus
    isFocused   = L.listName list == focusWidget

drawHelp :: Widget WidgetName
drawHelp = hBox [ C.hCenter $ vLimit 1 $ vBox [str "move: h/j/k/l; fetch: f; checkout: Enter; find: /; delete: d; rebase: r; quit: q" ]]
