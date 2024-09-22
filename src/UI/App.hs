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
import Text.Wrap


run :: IO ()
run = do
  initialState <- getInitialState
  _ <- defaultMain app initialState
  putStrLn "done"

app :: App State e String
app = App
  { appDraw         = drawMainScreen
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleUIEvent
  , appStartEvent   = return ()
  , appAttrMap      = const $ attrMap V.defAttr
      [ ( attrName "selected-line", V.rgbColor (255 :: Integer) 215 95 `on` V.rgbColor (38 :: Integer) (38 :: Integer) (38 :: Integer))
      , ( attrName "same-line", fg $ V.rgbColor (255 :: Integer) 215 95)
      , ( attrName "statusLine", V.rgbColor (255 :: Integer) 215 95 `on` V.rgbColor (38 :: Integer) (38 :: Integer) (38 :: Integer))
      ]
  }

lineStringSettings :: WrapSettings
lineStringSettings = WrapSettings 
  { preserveIndentation = True
  , breakLongWords = True
  , fillStrategy = NoFill
  , fillScope = FillAfterFirst
  }


drawMainScreen :: State -> [Widget String]
drawMainScreen state =
  [C.hCenter $ drawLocalBox state <+> drawRemoteBox state <=> drawStatusLine state <=> drawHelp]


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
drawStatusLine state = 
  padLeftRight 1 $ 
  vBox 
    [ withAttr (attrName "statusLine") $ vLimit 1 $ vBox [strWrap $ currentBranchNotification $ currentBranch state ]]


drawHelp :: Widget String
drawHelp = padLeftRight 1 $ vBox [ vLimit 1 $ vBox [strWrap "move: h/j/k/l; fetch: f; checkout: Enter; find: /; delete: d; rebase: r; quit: q" ]]


drawFocusedList :: (Int, Branch) -> (Int, Branch) -> Widget String
drawFocusedList selected focused =
  if selectedIndex == currentIndex
    then (withAttr (attrName "selected-line") . strWrapWith lineStringSettings) $ T.unpack $ printBranch $ snd focused
  else
    strWrapWith lineStringSettings $ T.unpack $ printBranch $ snd focused
  where
    currentIndex = fst focused
    selectedIndex = fst selected


drawNonFocusedList :: (Int, Branch) -> (Int, Branch) -> Widget String
drawNonFocusedList selected focused =
  if currentLabel == selectedLabel
    then (withAttr (attrName "same-line") . strWrapWith lineStringSettings) $ T.unpack $ printBranch $ snd focused
  else
    strWrapWith lineStringSettings $ T.unpack $ printBranch $ snd focused
  where
    currentLabel = branchLabel $ snd focused
    selectedLabel = branchLabel $ snd selected


currentBranchNotification :: Maybe Branch -> String
currentBranchNotification Nothing = "failed to detect current branch"
currentBranchNotification (Just branch) = "now on [" ++ T.unpack (branchName branch) ++ "] branch"


handleUIEvent :: BrickEvent String e -> EventM String State ()
handleUIEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleUIEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = modify selectNext
handleUIEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = modify selectPrevious
handleUIEvent (VtyEvent (V.EvKey (V.KChar 'l') [])) = modify selectRigth
handleUIEvent (VtyEvent (V.EvKey (V.KChar 'h') [])) = modify selectLeft
handleUIEvent (VtyEvent (V.EvKey V.KDown       [])) = modify selectNext
handleUIEvent (VtyEvent (V.EvKey V.KUp         [])) = modify selectPrevious
handleUIEvent (VtyEvent (V.EvKey V.KRight      [])) = modify selectRigth
handleUIEvent (VtyEvent (V.EvKey V.KLeft       [])) = modify selectLeft
handleUIEvent (VtyEvent (V.EvKey (V.KChar '/') [])) = return ()
handleUIEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = return ()
handleUIEvent (VtyEvent (V.EvKey (V.KChar 'd') [])) = do
  s <- get
  suspendAndResume $ do
    putStrLn "deleting ..."
    delete s
handleUIEvent (VtyEvent (V.EvKey V.KEnter      [])) = do
  s <- get 
  suspendAndResume $ do
    putStrLn "checkout ..."
    checkout s 
handleUIEvent (VtyEvent (V.EvKey (V.KChar 'f') [])) = do
  s <- get
  suspendAndResume $ do
    putStrLn "fetching ..."
    fetch s
handleUIEvent _ = return ()
