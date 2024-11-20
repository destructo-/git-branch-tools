{-# LANGUAGE OverloadedStrings #-}
module Handler
( eventHandler
) where

import qualified Brick.BChan        as BC
import qualified Graphics.Vty       as V
import qualified Brick.Widgets.List as L

import Brick
import Model.State
import Model.WidgetName
import Control.Lens.Operators
import Service.StateService
import Control.Monad.State (liftIO)
import Control.Concurrent (forkIO)
import Model.ApplicationEvent
import Model.Action
import Graphics.Vty

data BranchListType = LocalType | RemoteType

eventHandler :: BC.BChan ApplicationEvent -> BrickEvent WidgetName ApplicationEvent -> EventM WidgetName State ()
eventHandler _ ev@(AppEvent _) = handleApplicationEvent ev
eventHandler channel (VtyEvent ev) = do
  state <- get
  case state ^. focus of
    Local  -> handleBranchList channel LocalType ev 
    Remote -> handleBranchList channel RemoteType ev
eventHandler _ _ = return ()

handleBranchList :: BC.BChan ApplicationEvent -> BranchListType -> Event -> EventM WidgetName State ()
handleBranchList _ _ (V.EvKey (V.KChar 'q') []) = halt
handleBranchList _ _ (V.EvKey (V.KChar 'l') []) = modify $ setFocus stateService Remote
handleBranchList _ _ (V.EvKey (V.KChar 'h') []) = modify $ setFocus stateService Local
handleBranchList c _ (V.EvKey (V.KChar 'f') []) = emitEventAndUpdateAction c Fetch FetchStart
handleBranchList c _ (V.EvKey V.KEnter [])      = emitEventAndUpdateAction c Checkout CheckoutStart
handleBranchList c LocalType   (V.EvKey (V.KChar 'd') []) = emitEventAndUpdateAction c Delete DeleteStart
handleBranchList _ LocalType   ev = zoom localBranchList  $ L.handleListEventVi L.handleListEvent ev
handleBranchList _ RemoteType  ev = zoom remoteBranchList $ L.handleListEventVi L.handleListEvent ev

handleApplicationEvent :: BrickEvent WidgetName ApplicationEvent -> EventM WidgetName State ()
handleApplicationEvent (AppEvent Fetch) = do
  state   <- get
  state'  <- liftIO $ fetch stateService state
  put $ setAction stateService FetchFinish state'
handleApplicationEvent (AppEvent Checkout) = do
  state  <- get
  state' <- liftIO $ checkOut stateService state 
  put $ setAction stateService CheckoutFinish state'
handleApplicationEvent (AppEvent Delete) = do
  state  <- get
  state' <- liftIO $ deleteLocal stateService state
  put $ setAction stateService DeleteFinish state'
handleApplicationEvent _ = return ()

emitEventAndUpdateAction :: BC.BChan ApplicationEvent -> ApplicationEvent -> Action -> EventM WidgetName State ()
emitEventAndUpdateAction channel event newAction = do
  _ <- liftIO $ forkIO $ BC.writeBChan channel event
  modify $ setAction stateService newAction
