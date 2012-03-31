{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Users ( UsersController(..), userEdit, userUpdate ) where

import Control.Monad

import Models
import Layouts
import Utils
import Policy.Gitstar
import Views.Users

import LIO
import LIO.DCLabel

import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.IterIO.Http
import Data.IterIO.Http.Support
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import Hails.Data.LBson (Binary(..), genObjectId,genObjectId)

-- | Usercontroller
data UsersController = UsersController

instance RestController DC UsersController where
  -- /:id where :id is the user name
  restShow _ uName = do
    policy <- liftLIO gitstar
    muser <- liftLIO $ findBy policy "users" "_id" $ L8.unpack uName
    with404orJust muser $ \user -> do
      projs <- liftLIO $ mapM (findBy policy "projects" "_id") (userProjects user)
      let projects = catMaybes projs
      renderHtml $ showUser user projects

userEdit :: Action t DC ()
userEdit = do
  policy <- liftLIO gitstar
  uName <- getHailsUser
  user <- liftLIO $ getOrCreateUser uName
  renderHtml $ editUser user

userUpdate :: Action t DC ()
userUpdate = do
  policy   <- liftLIO gitstar
  uName    <- getHailsUser
  oldUser  <- liftLIO $ getOrCreateUser uName
  fullName <- paramToMStr "full_name"
  city     <- paramToMStr "city"
  website  <- paramToMStr "website"
  gravatar <- paramToMStr "gravatar"
  let user = oldUser { userFullName = fullName
                     , userCity = city
                     , userWebsite = website
                     , userGravatar = gravatar }
  privs <- appGetPolicyPriv policy
  liftLIO $ saveRecordP privs policy user
  redirectTo $ "/" ++ userName user
    where paramToMStr = getMParamVal L8.unpack

