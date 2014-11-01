module Main where

import Hails.HttpServer
import Hails.HttpServer.Auth
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.MethodOverride
import Gitstar (server)
import System.Environment

main :: IO ()
main = do
  let port = 3000
      app conf req = server conf req
  setEnv "DATABASE_CONFIG_FILE" "database.conf"
  runSettings (setPort port defaultSettings) $
    logStdoutDev $ execHailsApplication (devBasicAuth . methodOverride) app
