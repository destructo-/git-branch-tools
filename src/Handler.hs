module Handler
( eventHandler
) where

import qualified Graphics.Vty       as V
import qualified Brick.Widgets.List as L

import Brick
import Model.State
import Model.WidgetName
import Control.Lens.Operators
import Service.StateService
import Control.Monad.State (liftIO)

data BranchListType = LocalType | RemoteType

eventHandler :: BrickEvent WidgetName e -> EventM WidgetName State ()
eventHandler ev = do
  state <- get
  case state ^. focus of
    Local  -> handleBranchList LocalType ev 
    Remote -> handleBranchList RemoteType ev

handleBranchList :: BranchListType -> BrickEvent WidgetName e -> EventM WidgetName State ()
handleBranchList _ (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleBranchList _ (VtyEvent (V.EvKey (V.KChar 'l') [])) = modify $ setFocus stateService Remote
handleBranchList _ (VtyEvent (V.EvKey (V.KChar 'h') [])) = modify $ setFocus stateService Local
handleBranchList _ (VtyEvent (V.EvKey (V.KChar 'f') [])) = do
  state  <- get
  state' <- liftIO $ fetch stateService state
  put state'
handleBranchList _ (VtyEvent (V.EvKey V.KEnter [])) = do
  state  <- get
  state' <- liftIO $ checkOut stateService state 
  put state'
handleBranchList LocalType (VtyEvent (V.EvKey (V.KChar 'd') [])) = do
  state  <- get
  state' <- liftIO $ deleteLocal stateService state
  put state'
handleBranchList LocalType  (VtyEvent ev) = zoom localBranchList  $ L.handleListEventVi L.handleListEvent ev
handleBranchList RemoteType (VtyEvent ev) = zoom remoteBranchList $ L.handleListEventVi L.handleListEvent ev
handleBranchList _ _ = return ()
