{-# LANGUAGE Safe #-}
module Gitstar where

import Data.Monoid
import Hails.App
import Controllers
import Data.IterIO.Http.Support.Action

server :: AppReqHandler
server = runLHttpRoute $ mconcat 
    [ routeRestController "projects" ProjectsController
    , routeActionPattern "/:user_name/keys" $ listKeys
    , routeRestController "keys" KeysController
    , routeName "css" $ routeFileSys systemMimeMap (dirRedir "index.html") "static/css"
    , routeActionPattern "/:user_name" $ userShow
    ]
