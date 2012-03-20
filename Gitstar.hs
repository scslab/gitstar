{-# LANGUAGE Safe #-}
module Gitstar where

import Data.Monoid
import Hails.App
import Controllers

server :: AppReqHandler
server = do
  runLHttpRoute $ mconcat 
    [ routeRestController "projects" ProjectsController
    , routeFileSys systemMimeMap (dirRedir "index.html") "static"
    ]
