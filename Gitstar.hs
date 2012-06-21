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
import Control.Monad (void)

server :: AppReqHandler
server = runAction $ do
  req <- getHttpReq
  prms0 <- params
  body <- getBody >>= (liftLIO . unlabel)
  prms1 <- parseParams' req body
  void . setParams $ prms1 ++ prms0
  runActionRoute $ mconcat
    [ routeTop $ routeAction welcome
{- In dev mode:
    , routeMethod "GET" $ routePattern "/login" $
        routeAction (withUserOrDoAuth $ const redirectTo "/")
-}
    , routeMethod "GET" $ routePattern "/logout" $ routeAction goodbye
    , routeRestController "apps" AppsController
    , routeMethod "POST" $ routePattern "/keys/delete" $
        routeAction $ restDestroy KeysController undefined
    , routeRestController "keys" KeysController
    , routeMethod "GET" $ routePattern "/user/edit" $ routeAction userEdit
    , routeMethod "POST" $ routePattern "/user" $ routeAction userUpdate
    , routeRestController "users" UsersController
    , routeRestController "projects" ProjectsController
    , routeMethod "GET" $ routePattern "/:user_name" $ mconcat
      [ routeName "keys" $ routeAction listKeys
      , routePattern "/:id/edit" $ to restEdit ProjectsController]
    , routeMethod "GET" $ routePattern "/:user_name/:id" $
                          to restShow ProjectsController
    , routeMethod "POST" $ routePattern "/:user_name/:id" $
                           to restUpdate ProjectsController
    , routePattern "/:id" $ to restShow UsersController
    ]
      where to fn ctr = routeAction $
                          do (Just var) <- param $ pack "id"
                             fn ctr $ paramValue var
