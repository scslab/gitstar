{-# LANGUAGE Safe #-}
module Gitstar where

import Data.ByteString.Char8
import Data.Monoid
import Hails.App
import Controllers
import Data.IterIO.Http.Support.Action

toRestShow :: RestController m a => a -> Action t m ()
toRestShow ctr = do
  (Just var) <- param $ pack "id"
  restShow ctr $ paramValue var

server :: AppReqHandler
server = runLHttpRoute $ mconcat 
    [ routeName "css" $ routeFileSys systemMimeMap (dirRedir "index.html") "static/css"
    , routeName "js" $ routeFileSys systemMimeMap (dirRedir "index.html") "static/js"
    , routeRestController "projects" ProjectsController
    , routeRestController "keys" KeysController
    , routeActionPattern "/:user_name/keys" $ listKeys
    , routeActionPattern "/:user_name/:id" $ toRestShow ProjectsController
    , routeActionPattern "/:user_name" $ userShow
    ]
