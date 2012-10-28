{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Welcome ( welcome, goodbye ) where

import Layouts
import Views.Welcome

import Hails.HttpServer
import Hails.Web.Controller
import Hails.Web.User

import Utils

import Gitstar.Policy

welcome :: Controller Response
welcome = do
  musr <- getHailsUser
  case musr of
    Just usr -> getOrCreateUser usr >> return ()
    Nothing -> return ()
  homeHtml $ welcomeView musr

goodbye :: Controller Response
goodbye = withUserOrDoAuth $ \usr -> do
  homeHtml $ goodbyeView usr

