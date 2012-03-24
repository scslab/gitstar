{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Users ( KeysController(..), listKeys
                         , UsersController(..), userEdit
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

data KeysController = KeysController

contentType :: Monad m => Action t m S8.ByteString
contentType = do
  mctype <- requestHeader "accept"
  return $ fromMaybe "text/plain" mctype

listKeys :: Action t DC  ()
listKeys = do
    (Just uName) <- param "user_name" >>= return . (fmap (L8.unpack . paramValue))
    keys <- liftLIO $ fmap userKeys $ getOrCreateUser uName
    ctype <- contentType
    case ctype of
      "application/json" -> render "application/json" $ L8.pack $ keysToJson keys
      _ -> renderHtml $ keysIndex keys
    where keysToJson ks = "[" ++ (joinS $ map (unbin . sshKeyValue) ks) ++ "]"
          unbin (Binary bs) = S8.unpack bs
          joinS [] = ""
          joinS (x:[]) = show x
          joinS (x:xs) = (show x) ++ "," ++ (joinS xs)

instance RestController DC KeysController where
  restIndex _ = do
    uName <- (S8.unpack . fromJust) `liftM` requestHeader "x-hails-user"
    keys <- liftLIO $ fmap userKeys $ getOrCreateUser uName
    ctype <- contentType
    case ctype of
      "application/json" -> render "application/json" $ "[]"
      _ -> renderHtml $ keysIndex keys

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

