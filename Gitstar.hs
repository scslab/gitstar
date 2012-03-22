{-# LANGUAGE Safe #-}
module Gitstar where

import Data.Monoid
import Hails.App
import Controllers

server :: AppReqHandler
server = runLHttpRoute $ mconcat 
    [ routeRestController "projects" ProjectsController
    , routeRestController "users"    UsersController
    , routeFileSys systemMimeMap (dirRedir "index.html") "static"
    ]
