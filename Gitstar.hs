module Gitstar where

import Data.Monoid
import Hails.IterIO.HailsRoute
import Data.IterIO.Http.Support.RestController
import Data.IterIO.Http.Support.Routing
import Data.IterIO.Http
import LIO.DCLabel

import Controllers

server :: DCPrivTCB -> HttpRequestHandler DC ()
server priv = do
  runLHttpRoute $ mconcat [ routeRestController "users" UsersController
													, routeRestController "messages" MessagesController
                          , routeFileSys systemMimeMap (dirRedir "index.html") "static" ]

