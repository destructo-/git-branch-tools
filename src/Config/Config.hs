module Config.Config where

import qualified Brick.Widgets.List as List

import Brick.Themes
import Graphics.Vty
import Brick

listElementHeigth :: Int
listElementHeigth = 1

theme :: Theme
theme =
  newTheme defAttr 
    [ (List.listAttr, fg white)
    , (List.listSelectedAttr, fg defYellow)
    , (List.listSelectedFocusedAttr, defYellow `on` defDarkGray)
    , (attrStatsLine, fg defYellow)
    , (attrActionLine, withStyle (fg white) bold)
    ]

-- #ffd75f
defYellow :: Color
defYellow = rgbColor (255 :: Integer) 215 95

-- #262626
defDarkGray :: Color
defDarkGray = rgbColor (38 :: Integer) 38 38

attrStatsLine :: AttrName
attrStatsLine = attrName "status-line"

attrActionLine :: AttrName
attrActionLine = attrName "action-line"
