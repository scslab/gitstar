{-# LANGUAGE Safe #-}
module Gitstar where

import Data.Monoid
import Hails.App
import Controllers
import Data.IterIO.Http.Support.Action

server :: AppReqHandler
server = runLHttpRoute $ mconcat 
    [ routeRestController "projects" ProjectsController
    , routeActionPattern "/users/:uid/keys/:kid/new" newUserKeyController
    , routeActionPattern "/users/:uid" userShowController
    --, routeRestController "users"    UsersController
    , routeFileSys systemMimeMap (dirRedir "index.html") "static"
    ]
