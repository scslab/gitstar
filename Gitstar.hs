module Gitstar where

import Data.Monoid
import Hails.App
import Controllers

server :: AppReqHandler
server = do
  runLHttpRoute $ mconcat 
    [ routeRestController "users" UsersController
    , routeRestController "messages" MessagesController
    , routeFileSys systemMimeMap (dirRedir "index.html") "static"
    ]
