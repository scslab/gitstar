{-# LANGUAGE Safe #-}
module Gitstar where

import Data.Monoid
import Hails.App
import Controllers
import Data.IterIO.Http.Support.Action

server :: AppReqHandler
server = runLHttpRoute $ mconcat 
    [ routeRestController "projects" ProjectsController
    , routeActionPattern "/users/:uid" userShowController
    , routeActionPattern "/users/:uid/keys/:kid/new" newUserKeyController
    --, routeRestController "users"    UsersController
    , routeFileSys systemMimeMap (dirRedir "index.html") "static"
    ]
