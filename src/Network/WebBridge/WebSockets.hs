{-# LANGUAGE GADTs #-}

module Network.WebBridge.WebSockets where

import Network.WebBridge.Internal

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad.Operational
import Control.Monad.IO.Class

import qualified Data.IntMap as IntMap
import qualified Data.Aeson as JSON
import qualified Network.WebSockets as WS

runWebSocketServer :: WS.TextProtocol v => Server a -> WS.Request -> WS.WebSockets v ()
runWebSocketServer (Server server) req = do
    WS.acceptRequest req

    reqMap   <- liftIO $ newMVar $ IntMap.empty
    incoming <- liftIO $ newChan
    sink     <- WS.getSink

    let eval p = case view p of
            Return a -> return ()
            LiftIO op :>>= cont -> op >>= eval . cont
            EvalWait c :>>= cont -> do
                WS.sendSink sink $ WS.DataMessage $ WS.Text $ JSON.encode (ReqEval c 0)
                bs <- readChan incoming
                let Just (Response a _) = JSON.decode' bs
                eval . cont $ a

        receiveLoop = do
            WS.Text bs <- WS.receiveDataMessage
            liftIO $ writeChan incoming bs
            receiveLoop

    liftIO $ forkIO $ eval server
    receiveLoop

