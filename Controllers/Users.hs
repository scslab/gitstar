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
  restShow _ uName = do
    policy <- liftLIO gitstar
    muser <- liftLIO $ findBy policy "users" "_id" $ L8.unpack uName
    case muser of
      Just user -> do
        projs <- liftLIO $ mapM (findBy policy "projects" "_id") (userProjects user)
        let projects = map (fromMaybe undefined) projs
        renderHtml $ showUser user projects
      Nothing -> respond404

userEdit :: Action t DC ()
userEdit = do
  policy <- liftLIO gitstar
  uName <- (S8.unpack . fromJust) `liftM` requestHeader "x-hails-user"
  user <- liftLIO $ getOrCreateUser uName
  renderHtml $ editUser user

userUpdate :: Action t DC ()
userUpdate = do
  policy <- liftLIO gitstar
  uName <- (S8.unpack . fromJust) `liftM` requestHeader "x-hails-user"
  oldUser <- liftLIO $ getOrCreateUser uName
  fullName <- param "full_name" >>= return . (fmap (L8.unpack . paramValue))
  city <- param "city" >>= return . (fmap (L8.unpack . paramValue))
  website <- param "website" >>= return . (fmap (L8.unpack . paramValue))
  gravatar <- param "gravatar" >>= return . (fmap (L8.unpack . paramValue))

  let user = oldUser {
      userFullName = fullName
    , userCity = city
    , userWebsite = website
    , userGravatar = gravatar
  }
  privs <- doGetPolicyPriv policy
  liftLIO $ saveRecordP privs policy user
  redirectTo $ "/" ++ userName user

