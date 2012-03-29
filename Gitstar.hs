{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
module Gitstar where

import Data.ByteString.Char8
import Data.Monoid
import Hails.App
import Controllers
import Data.IterIO.Http.Support

server :: AppReqHandler
server = runLHttpRoute $ mconcat 
    [ routeTop $ routeAction welcome
    , routeName "static" $
        routeFileSys systemMimeMap (dirRedir "index.html") "static"
    --
    , routeMethod "GET" $ routeActionPattern
                            "/repos/:user_name/:project_name/branches"
                            repoShowBranches
    --
    , routeRestController "keys" KeysController
    , routeMethod "GET" $ routeActionPattern "/user/edit" userEdit
    , routeMethod "POST" $ routeActionPattern "/user" userUpdate
    , routeRestController "projects" ProjectsController
    , routeMethod "GET" $ routeActionPattern "/:user_name/keys" listKeys
    , routeMethod "GET" $ routeActionPattern "/:user_name/:id/edit" $
                          to restEdit ProjectsController
    , routeMethod "GET" $ routeActionPattern "/:user_name/:id" $
                          to restShow ProjectsController
    , routeMethod "POST" $ routeActionPattern "/:user_name/:id" $
                           to restUpdate ProjectsController
    , routeActionPattern "/:id" $ to restShow UsersController
    ]
      where to fn ctr = do (Just var) <- param $ pack "id"
                           fn ctr $ paramValue var
