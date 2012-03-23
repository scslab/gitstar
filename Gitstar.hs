{-# LANGUAGE Safe #-}
module Gitstar where

import Data.Monoid
import Hails.App
import Controllers
import Data.IterIO.Http.Support.Action

server :: AppReqHandler
server = runLHttpRoute $ mconcat 
    [ routeRestController "projects" ProjectsController
    , routeRestController "keys" KeysController
    , routeFileSys systemMimeMap (dirRedir "index.html") "static"
    ]
