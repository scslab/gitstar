{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Keys ( KeysController(..), listKeys ) where

import Control.Monad

import Models
import Layouts
import Utils
import Policy.Gitstar
import Views.Keys

import LIO
import LIO.DCLabel

import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.IterIO.Http
import Data.IterIO.Http.Support
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Hails.Data.LBson hiding (map)

data KeysController = KeysController

contentType :: Monad m => Action t m S8.ByteString
contentType = do
  mctype <- requestHeader "accept"
  return $ fromMaybe "text/plain" mctype

listKeys :: Action t DC  ()
listKeys = do
  uName <- getParamVal "user_name"
  doListKeys uName

doListKeys :: UserName -> Action t DC  ()
doListKeys uName = do
  keys <- liftLIO $ fmap userKeys $ getOrCreateUser uName
  atype <- requestHeader "accept"
  case atype of
    Just "application/bson" ->
      render "application/bson" $ encodeDoc $ mkDoc keys
    _ -> renderHtml $ keysIndex keys
    where convert = fromJust . safeToBsonDoc . toDocument 
          mkDoc ks = fromJust . safeToBsonDoc $
                      (["keys" =: map convert ks] :: Document DCLabel)

instance RestController DC KeysController where
  restIndex _ = do
    uName <- getHailsUser
    doListKeys uName

  restNew _ = renderHtml newUserKey

  restCreate _ = do
    uName <- getHailsUser
    user <- liftLIO $ getOrCreateUser uName
    keyTitle <- getParamVal "ssh_key_title"
    keyValue <- (paramValue . fromJust) `liftM` param "ssh_key_value"
    nId <- liftLIO $ genObjectId
    let key = SSHKey { sshKeyId = nId
                     , sshKeyTitle = keyTitle
                     , sshKeyValue = Binary $ strictify keyValue}
    let resultUser = user { userKeys = key : userKeys user }
    policy <- liftLIO gitstar
    privs <- appGetPolicyPriv policy
    liftIO $ saveRecordP privs policy resultUser
    redirectTo "/keys"
      where strictify = S.concat . L.toChunks

