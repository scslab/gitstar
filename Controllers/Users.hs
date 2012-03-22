{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Users ( UsersController(..) ) where

import Control.Monad

import Models
import Layouts
import Utils
import Policy.Gitstar (gitstar)
import Views.Users

import LIO
import LIO.DCLabel

import Data.Maybe (catMaybes, fromJust)
import Data.IterIO.Http
import Data.IterIO.Http.Support
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import Hails.Data.LBson (Binary(..))

-- | Usercontroller
data UsersController = UsersController

instance RestController DC UsersController where
  restShow _ uid = do
    policy <- liftLIO gitstar
    muser <- liftLIO $ findBy policy "users" "_id" (L8.unpack uid)
    case muser of
      Nothing -> respond404
      Just u  -> do mps <- liftLIO $ forM (userProjects u) $ \pid ->
                             findBy policy "projects" "_id" pid
                    renderHtml $ showUser u (catMaybes mps)

  restEdit _ uid = do
    policy <- liftLIO gitstar
    muser <- liftLIO $ findBy policy "users" "_id" (L8.unpack uid)
    maybe respond404 (renderHtml . editUser) muser

  restNew _ = do 
    policy <- liftLIO gitstar
    uName  <- (S8.unpack . fromJust) `liftM` requestHeader "x-hails-user"
    muser <- liftLIO $ findBy policy "users" "_id" uName
    maybe (renderHtml $ newUser uName) redirectToEdit muser
      where redirectToEdit u = redirectTo $ "/users/" ++ userName u ++ "/edit"

  restCreate _ = do
    policy <- liftLIO gitstar
    uName  <- (S8.unpack . fromJust) `liftM` requestHeader "x-hails-user"
    uKey   <- getParamVal "ssh_key"
    let user = User { userName     = uName
                    , userKey      = Binary . S8.pack $ uKey
                    , userProjects = []
                    } 
    privs <- doGetPolicyPriv policy
    erf <- liftLIO $ insertRecordP privs policy "users" user
    case erf of
      Right u -> redirectTo $ "/users/" ++ show u
      _       -> respondStat stat500

  restUpdate _ _ = do
    respond404
