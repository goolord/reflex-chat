{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import Common.Route
import Control.Concurrent
import Control.Concurrent (MVar, readMVar, modifyMVar_)
import Control.Monad      (forM_, forever)
import Data.Aeson
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Data.Text          (Text)
import Network.WebSockets.Snap
import Obelisk.Backend
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Network.WebSockets as WS

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      webSocketChatState <- newMVar newServerState
      serve $ \case
        BackendRoute_Missing :=> Identity () -> return ()
        BackendRoute_WebSocketChat :=> Identity () -> do
          runWebSocketsSnap (application webSocketChatState)

  , _backend_routeEncoder = backendRouteEncoder
  }

type Client = (WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

broadcast :: B.ByteString -> ServerState -> IO ()
broadcast message clients = do
  BC8.putStrLn message
  forM_ clients $ \conn -> WS.sendTextData conn message

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  modifyMVar_ state (pure . (:) conn)
  WS.forkPingThread conn 30
  msgbs <- WS.receiveData conn :: IO B.ByteString
  WS.sendTextData conn msgbs
  talk conn state

talk :: WS.Connection -> MVar ServerState -> IO b
talk conn state = forever $ do
  msgbs <- WS.receiveData conn :: IO B.ByteString
  readMVar state >>= broadcast msgbs

options :: Options
options = defaultOptions -- { tagSingleConstructors = True }

