{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Keys ( keysController, listKeys ) where

import Prelude hiding (show)

import Control.Monad

import Gitstar.Models
import Layouts
import Gitstar.Policy
import Views.Keys

import LIO

import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T

import Hails.HttpServer
import Hails.Data.Hson
import Hails.Database.Structured
import Hails.Web
import Hails.Web.REST

import Utils

-- | List keys for the user specified by parameter @user_name@
listKeys :: Controller Response
listKeys = do
  curUser <- fromMaybe "" `liftM` getHailsUser
  uNameQ <- queryParam "id"
  let uName = maybe curUser (T.pack . S8.unpack) uNameQ
  doListKeys False uName

-- | Given a labeled username actually list the keys for the user.
doListKeys :: Bool -> UserName -> Controller Response
doListKeys updateFlag uName = do
  keys  <- liftLIO $ userKeys `liftM` getOrCreateUser uName
  atype <- requestHeader "accept"
  case atype of
    Just "application/bson" ->
      return $ ok "application/bson" $ serialize $ mkDoc keys
    _ -> renderHtml $ keysIndex updateFlag keys
    where mkDoc :: [SSHKey] -> Document
          mkDoc ks = ["keys" -: map sshKeyToBson ks] :: Document

keysController :: RESTController
keysController = do
  index $ withUserOrDoAuth (doListKeys True)

  new $ withUserOrDoAuth $ \_ -> renderHtml newUserKey

  create $ withUserOrDoAuth $ \uName -> do
    lreq <- request
    liftLIO $ do
      ldoc <- labeledRequestToHson lreq
      luser  <- addUserKey uName ldoc
      withGitstar $ do
              saveLabeledRecord luser
              unlabel luser
    return $ redirectTo "/keys"

  delete $ withUserOrDoAuth $ \uName -> do
    lreq <- request
    liftLIO $ do
      --u0 <- getOrCreateUser uName
      ldoc <- labeledRequestToHson lreq
      luser  <- delUserKey uName ldoc
      withGitstar $ do
            saveLabeledRecord luser
            unlabel luser
    redirectBack
    {-if u0 == u1
      then flashError "User keys were not changed."
      else flashSuccess "Deleted key!"-}

