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

    reqId    <- liftIO $ newMVar (0 :: Int)
    reqMap   <- liftIO $ newMVar $ IntMap.empty
    sink     <- WS.getSink

    let eval p = case view p of
            Return a -> return ()
            LiftIO op :>>= cont -> op >>= eval . cont
            EvalWait c :>>= cont -> do
                i <- takeMVar reqId
                putMVar reqId (i+1)
                WS.sendSink sink $ WS.DataMessage $ WS.Text $ JSON.encode (ReqEval c i)
                rm <- takeMVar reqMap
                let handler val = do
                        let JSON.Success a = JSON.fromJSON val
                        eval . cont $ a
                putMVar reqMap $ IntMap.insert i handler rm

        receiveLoop = do
            WS.Text bs <- WS.receiveDataMessage
            let Just (Response val i) = JSON.decode' bs
            rm <- liftIO $ takeMVar reqMap
            let Just cont = IntMap.lookup i rm
                rm' = IntMap.delete i rm
            liftIO $ putMVar reqMap rm' >> forkIO (cont val)
            receiveLoop

    liftIO $ forkIO $ eval server
    receiveLoop

