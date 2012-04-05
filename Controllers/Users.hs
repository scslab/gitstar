{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Users ( UsersController(..), userEdit, userUpdate ) where

import Models
import Layouts
import Utils
import Policy.Gitstar
import Views.Users

import LIO
import LIO.DCLabel

import Data.Maybe (catMaybes)
import Data.IterIO.Http.Support
import qualified Data.ByteString.Lazy.Char8 as L8

import Hails.App

import Control.Monad (void)

-- | Usercontroller
data UsersController = UsersController

instance RestController t b DC UsersController where
  -- /:id where :id is the user name
  restShow _ uName = do
    policy <- liftLIO gitstar
    muser <- liftLIO $ findBy policy "users" "_id" $ L8.unpack uName
    with404orJust muser $ \user -> do
      -- findBy will return only the projects current app can see
      projs <- liftLIO $ mapM (findBy policy "projects" "_id") (userProjects user)
      let projects = catMaybes projs
      renderHtml $ showUser user projects

-- | Show edit form for user
userEdit :: Action t b DC ()
userEdit = do
  uName <- getHailsUser
  user <- liftLIO $ getOrCreateUser uName
  renderHtml $ editUser user

-- | Update user's profile
userUpdate :: Action t (DCLabeled L8.ByteString) DC ()
userUpdate = do
  policy <- liftLIO gitstar
  ldoc   <- bodyToLDoc 
  uName  <- getHailsUser
  void . liftLIO $ do luser <- partialUserUpdate uName ldoc
                      updateUser luser
  redirectTo $ "/" ++ uName

