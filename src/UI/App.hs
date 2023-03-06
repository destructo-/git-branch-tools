{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module UI.App(
  run
) where
import Brick
import qualified Brick.Widgets.Center        as C
import Brick.Widgets.Border
import qualified Graphics.Vty                as V
import Branch
import qualified Data.Text as T
import State


run :: IO ()
run = do
  initialState <- getInitialState
  _ <- defaultMain app initialState
  putStrLn "done!"


app :: App State e String
app = App
  { appDraw = drawMainScreen
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleUIEvent
  , appStartEvent   = pure
  , appAttrMap      = const $ attrMap V.defAttr
      [ ("selected", V.rgbColor (255 :: Integer) 215 95 `on` V.rgbColor (38 :: Integer) (38 :: Integer) (38 :: Integer))
      , ("same", fg $ V.rgbColor (255 :: Integer) 215 95)
      , ("operationLog", V.rgbColor (255 :: Integer) 215 95 `on` V.rgbColor (38 :: Integer) (38 :: Integer) (38 :: Integer))
      ]
  }


drawMainScreen :: State -> [Widget String]
drawMainScreen state = [C.hCenter $ drawLocalBox state <+> drawRemoteBox state <=> drawStatusLine state <=> drawLastOperationLog state <=> drawHelp]


drawLocalBox :: State -> Widget String
drawLocalBox state =
  padLeftRight 1 $
  vBox
    [ hBorderWithLabel $ padLeftRight 1 $ str "Local"
    , vBox $ map (drawListFunction $ selectedLeft state) $ localBranchList state
    , fill ' '
    ]
  where
    drawListFunction = case focus state of
      RightFocus -> drawNonFocusedList
      LeftFocus -> drawFocusedList


drawRemoteBox :: State -> Widget String
drawRemoteBox state =
  padLeftRight 1 $
  vBox
    [ hBorderWithLabel $ padLeftRight 1 $ str "Remote"
    , vBox $ map (drawListFunction $ selectedRigth state) $ remoteBranchList state
    , fill ' '
    ]
  where
    drawListFunction = case focus state of
      LeftFocus -> drawNonFocusedList
      RightFocus -> drawFocusedList


drawStatusLine :: State -> Widget String
drawStatusLine state = padAll 1 $ vBox [ withAttr "statusLine" $ vLimit 1 $ vBox [strWrap $ currentBranchNotification $ currentBranch state ]]

drawHelp :: Widget String
drawHelp = padLeftRight 1 $ vBox [ vLimit 1 $ vBox [strWrap "move: h/j/k/l; fetch: f; prune: p; checkout: Enter; find: /; delete: d; rebase: r; quit: q" ]]

drawLastOperationLog :: State -> Widget String
drawLastOperationLog state = padLeftRight 1 $ vBox [ withAttr "operationLog" $ vLimit 1 $ vBox [strWrap $ "Last operation: " ++ lastOperation state ]]


drawFocusedList :: (Int, Branch) -> (Int, Branch) -> Widget String
drawFocusedList selected focused =
  if selectedIndex == currentIndex
    then (withAttr "selected" . strWrap) $ T.unpack $ branchName $ snd focused
  else
    strWrap $ T.unpack $ branchName $ snd focused
  where
    currentIndex = fst focused
    selectedIndex = fst selected


drawNonFocusedList :: (Int, Branch) -> (Int, Branch) -> Widget String
drawNonFocusedList selected focused =
  if currentLabel == selectedLabel
    then (withAttr "same" . strWrap) $ T.unpack $ branchName $ snd focused
  else
    strWrap $ T.unpack $ branchName $ snd focused
  where
    currentLabel = branchLabel $ snd focused
    selectedLabel = branchLabel $ snd selected


currentBranchNotification :: Maybe Branch -> String
currentBranchNotification Nothing = "failed to detect current branch"
currentBranchNotification (Just branch) = "now on [" ++ T.unpack (branchName branch) ++ "] branch"


handleUIEvent :: State -> BrickEvent n e -> EventM n (Next State)
handleUIEvent state event =
  case event of
    VtyEvent vtype ->
      case (vtype, state) of
        (V.EvKey (V.KChar 'q') [], _) -> halt state

        (V.EvKey (V.KChar 'j') [], _) -> continue $ selectNext state
        (V.EvKey (V.KChar 'k') [], _) -> continue $ selectPrevious state
        (V.EvKey (V.KChar 'l') [], _) -> continue $ selectRigth state
        (V.EvKey (V.KChar 'h') [], _) -> continue $ selectLeft state
        (V.EvKey V.KDown       [], _) -> continue $ selectNext state
        (V.EvKey V.KUp         [], _) -> continue $ selectPrevious state
        (V.EvKey V.KRight      [], _) -> continue $ selectRigth state
        (V.EvKey V.KLeft       [], _) -> continue $ selectLeft state

        (V.EvKey (V.KChar 'f') [], _) -> suspendAndResume $ fetch state
        (V.EvKey (V.KChar 'p') [], _) -> suspendAndResume $ prune state
        (V.EvKey (V.KChar 'r') [], _) -> suspendAndResume $ pure state
        (V.EvKey (V.KChar 'd') [], _) -> suspendAndResume $ pure state
        (V.EvKey (V.KChar '/') [], _) -> suspendAndResume $ pure state
        (V.EvKey V.KEnter      [], _) -> suspendAndResume $ checkout state

        _ -> continue state
    _ -> continue state
