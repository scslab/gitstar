{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
#define DEVELOPMENT
{-# LANGUAGE OverloadedStrings #-}
module Gitstar where

import Hails.HttpServer
import Hails.Web.Router
import Hails.Web.Responses
import Controllers

import Utils

server :: Application
server = mkRouter $ do
    routeTop welcome
    routeMethod GET $ routeName "login" $
      withUserOrDoAuth $ const $ return $ redirectTo "/"
    routeMethod GET $ routeName "logout" goodbye
    routeName "apps" appsController
    routeName "keys" keysController
    routeMethod GET $ routeName "user" $ routeName "edit" userEdit
    routeMethod PUT $ routeName "user" userUpdate
    routeName "users" usersController
    routeName "projects" projectsController

    routeVar "id" $ do
      routeTop $ userShow
      routeName "keys" listKeys
    routeVar "user_name" projectsController
