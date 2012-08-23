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
    reqMap   <- liftIO $ newMVar IntMap.empty
    sink     <- WS.getSink

    let eval :: Program ServerInstr a -> IO ()
        eval p = case view p of
            Return a -> return ()
            LiftIO op :>>= cont -> op >>= eval . cont
            NextReqId :>>= cont -> do
                i <- takeMVar reqId
                putMVar reqId $! (i+1)
                eval . cont $ i
            SendReq req :>>= cont -> do
                WS.sendSink sink $ WS.DataMessage $ WS.Text $ JSON.encode req
                eval . cont $ ()
            WaitForRes rqId _ :>>= cont -> do
                rm <- takeMVar reqMap
                putMVar reqMap $ IntMap.insert rqId (eval . cont) rm

        receiveLoop = do
            WS.Text bs <- WS.receiveDataMessage
            let Just res@(Response _ i) = JSON.decode' bs
            rm <- liftIO $ takeMVar reqMap
            let Just cont = IntMap.lookup i rm
                rm' = IntMap.delete i rm
            liftIO $ putMVar reqMap rm' >> forkIO (cont res)
            receiveLoop

    liftIO $ forkIO $ eval server
    receiveLoop

