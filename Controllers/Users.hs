{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Users ( usersController, userShow, userEdit, userUpdate ) where

import Prelude hiding ((++), show)

import Layouts
import Gitstar.Models
import Gitstar.Policy
import Views.Users

import LIO

import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T

import Hails.HttpServer
import Hails.Database
import Hails.Database.Structured
import Hails.Web
import Hails.Web.REST

import Utils

userShow :: Controller Response
userShow = do
  (Just uName) <- queryParam "id"
  muser <- liftLIO $ withGitstar $ findBy "users" "_id" $ S8.unpack uName
  with404orJust muser $ \user -> do
    -- findBy will return only the projects current app can see
    projs <- liftLIO $ withGitstar $
      mapM (findBy "projects" "_id") (userProjects user)
    let projects = catMaybes projs
    renderHtml $ showUser user projects

-- | Usercontroller
usersController :: RESTController
usersController = do
  index $ do
    query <- queryParam "q"
    users <- liftLIO $ withGitstar $
      findAll $ select ["_id" -: query] "users"
    renderHtml $ listUsers users

  -- /:id where :id is the user name
  show userShow

-- | Show edit form for user
userEdit :: Controller Response
userEdit = withUserOrDoAuth $ \uName -> do
  user <- liftLIO $ getOrCreateUser uName
  renderHtml $ editUser user

-- | Update user's profile
userUpdate :: Controller Response
userUpdate = withUserOrDoAuth $ \uName -> do
  lreq <- request
  let ldoc = labeledRequestToHson lreq
  luser  <- liftLIO $ partialUserUpdate uName ldoc
  withGitstar $ do
          saveLabeledRecord luser
  respond $ redirectTo $ T.unpack $ "/" ++ uName

