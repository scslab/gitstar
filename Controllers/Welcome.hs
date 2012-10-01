{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Welcome ( welcome, goodbye ) where

import LIO

import Layouts
import Views.Welcome

import Hails.HttpServer
import Hails.Web.Controller

import qualified Data.ByteString.Char8 as S8
import System.FilePath (takeExtensions)

import Utils

welcome :: Controller Response
welcome = do
  usr <- getHailsUser
  homeHtml $ welcomeView usr

goodbye :: Controller Response
goodbye = withUserOrDoAuth $ \usr -> do
  req <- request >>= liftLIO . unlabel
  let domain = Just $ takeExtensions $ S8.unpack $ serverName req
--  delCookie "_hails_user" domain
--  delCookie "_hails_user_hmac" domain
  homeHtml $ goodbyeView usr

