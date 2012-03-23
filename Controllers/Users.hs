{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Users (KeysController(..)
                         ) where

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

{-
  restEdit _ uid = do
    policy <- liftLIO gitstar
    muser <- liftLIO $ findBy policy "users" "_id" (L8.unpack uid)
    mkeyName <- param "ssh_key_name"
    case (muser, mkeyName) of
      (Just u, Just n) -> 
        let keyN = L8.unpack .  paramValue $ n
            keyV = lookup keyN $ keysToPairs $ userKeys u
        in maybe respond404 (renderHtml . editUserKey (userName u)) keyV
      _ -> respond404
    where keysToPairs = map (\k@(SSHKey n _) -> (n,k))

  restNew _ = do
    -- Reedirec to the the key controller
    uName <- (S8.unpack . fromJust) `liftM` requestHeader "x-hails-user"
    kid <- liftLIO $ genObjectId
    redirectTo $ "/users/" ++ uName ++  "/keys/" ++ show kid ++ "/new"
-}

{-
  restCreate _ = do
    policy <- liftLIO gitstar
    uName  <- (S8.unpack . fromJust) `liftM` requestHeader "x-hails-user"
    uKeyN  <- getParamVal "ssh_key_name"
    uKeyV  <- getParamVal "ssh_key_val"
    muser  <- liftLIO $ findBy policy "users" "_id" uName
    let new_key = SSHKey uKeyN (Binary . S8.pack $ uKeyV)
        user = User { userName     = uName
                    , userKeys     = new_key : maybe [] userKeys muser
                    , userProjects = maybe [] userProjects muser }
    privs <- doGetPolicyPriv policy
    erf <- liftLIO $ saveRecordP privs policy "users" user
    case erf of
      Right _ -> redirectTo $ "/users/" ++ uName
      _       -> respondStat stat500
-}

{-
  restUpdate _ _ = do
    policy <- liftLIO gitstar
    uName  <- (S8.unpack . fromJust) `liftM` requestHeader "x-hails-user"
    muser <- liftLIO $ findBy policy "users" "_id" uName
    case muser of
     Nothing -> redirectTo $ "/users/new"
     Just u -> do uKey   <- getParamVal "ssh_key"
                  let user = u --{ userKey  = Binary . S8.pack $ uKey } 
                  privs <- doGetPolicyPriv policy
                  erf <- liftLIO $ insertRecordP privs policy "users" user
                  case erf of
                    Right u -> redirectTo $ "/users/" ++ show u
                    _       -> respondStat stat500
                    -}
data KeysController = KeysController

instance RestController DC KeysController where
  restIndex _ = do
    uName <- (S8.unpack . fromJust) `liftM` requestHeader "x-hails-user"
    keys <- liftLIO $ fmap userKeys $ getOrCreateUser uName
    renderHtml $ keysIndex keys

  restNew _ = do
    renderHtml $ newUserKey

  restCreate _ = do
    uName <- (S8.unpack . fromJust) `liftM` requestHeader "x-hails-user"
    user <- liftLIO $ getOrCreateUser uName
    keyTitle <- param "ssh_key_title" >>= return . fmap paramValue >>= return . fromMaybe ""
    keyValue <- param "ssh_key_value" >>= return . fmap paramValue >>= return . fromMaybe ""
    let key = SSHKey { sshKeyTitle = L8.unpack keyTitle
                     , sshKeyValue = Binary $ S8.pack $ L8.unpack keyValue}
    -- TODO: validate key title/value aren't empty
    let resultUser = user { userKeys = key:(userKeys user) }
    policy <- liftLIO gitstar
    privs <- doGetPolicyPriv policy
    liftIO $ saveRecordP privs policy resultUser
    redirectTo "/keys"

