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
import Utils
import Views.Welcome

import Hails.App
import LIO.DCLabel

import qualified Data.ByteString.Char8 as S8
import Data.IterIO.Http
import Data.IterIO.Http.Support
import System.FilePath (takeExtensions)

welcome :: Action t b DC ()
welcome = renderHtml welcomeView

goodbye :: Action t b DC ()
goodbye = do
  usr <- getHailsUser
  req <- getHttpReq
  let domain = Just $ takeExtensions $ S8.unpack $ reqHost req
  delCookie "_hails_user" domain
  delCookie "_hails_user_hmac" domain
  renderHtml $ goodbyeView usr
