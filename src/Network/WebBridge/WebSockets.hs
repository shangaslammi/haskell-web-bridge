{-# LANGUAGE GADTs #-}

module Network.WebBridge.WebSockets where

import Network.WebBridge.Internal

import Control.Monad.Operational
import Control.Monad.IO.Class

import qualified Data.Aeson as JSON
import qualified Network.WebSockets as WS

runWebSocketServer :: WS.TextProtocol v => Server a -> WS.Request -> WS.WebSockets v ()
runWebSocketServer (Server server) req = WS.acceptRequest req >> (eval . view $ server) where
    eval p = case p of
        Return a -> return ()
        instr :>>= cont -> case instr of
            LiftIO op -> liftIO $ op
            EvalWait c -> do
                WS.sendTextData $ JSON.encode (ReqEval c 0)
                WS.Text bs <- WS.receiveDataMessage
                let Just (Response a _) = JSON.decode' bs
                return a
            >>= eval . view . cont
