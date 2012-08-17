{-# LANGUAGE GADTs #-}

module Network.WebBridge.WebSockets where

import Network.WebBridge.Internal

import Control.Monad.Operational
import Control.Monad.IO.Class

import qualified Data.Aeson as JSON
import qualified Network.WebSockets as WS

runWebSocketServer :: WS.TextProtocol v => Server a -> WS.Request -> WS.WebSockets v ()
runWebSocketServer (Server server) req = do
    let eval p = case view p of
            Return a -> return ()
            LiftIO op :>>= cont -> liftIO op >>= eval . cont
            EvalWait c :>>= cont -> do
                WS.sendTextData $ JSON.encode (ReqEval c 0)
                WS.Text bs <- WS.receiveDataMessage
                let Just (Response a _) = JSON.decode' bs
                eval . cont $ a

    WS.acceptRequest req
    eval server

