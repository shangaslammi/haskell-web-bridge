{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Network.WebBridge.Internal
import Network.WebBridge.JSAPI
import Network.WebBridge.WebSockets

import Control.Monad.IO.Class

import qualified Data.Text.Lazy.IO as T
import qualified Network.WebSockets as WS
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import Data.FileEmbed (embedDir)

import Prelude hiding ((++))

runWarp :: IO ()
runWarp = do
    let app = runWebSocketServer test :: WS.Request -> WS.WebSockets WS.Hybi00 ()
    Warp.runSettings Warp.defaultSettings
        { Warp.settingsPort = 8000
        , Warp.settingsIntercept = WaiWS.intercept app
        } staticApp

staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "static")

main = runWarp

promptName :: Client (JS String)
promptName = do
    name <- prompt "What's your name"
    alert $ "Hello " ++ name
    return name

test :: Server ()
test = do
    liftIO $ T.putStrLn "Web client connected"

    name <- onClient promptName

    liftIO $ T.putStrLn $ "Client's name is " ++ name

