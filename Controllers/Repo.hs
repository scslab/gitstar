{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controllers.Repo {-( welcome ) -}where

import Layouts
import Policy.Gitstar
import Config
import Utils

import Data.Maybe
import Control.Monad

import LIO
import LIO.DCLabel

import Hails.Database.MongoDB (select, (=:))
import Hails.Database.MongoDB.Structured

import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.Http.Support
import Hails.IterIO.HttpClient

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

repoShowBranches :: Action t DC ()
repoShowBranches = do
  policy <- liftLIO gitstar
  uName  <- getParamVal "user_name"
  pName  <- getParamVal "project_name"
  mProj  <- liftLIO $ do
    -- Make sure current user can read:
    findWhere policy $ select [ "name"  =: pName, "owner" =: uName ] "projects"
  with404orJust mProj $ \(_ :: Project) -> do
    acceptHdrVal <- getAccept
    let url = gitstar_ssh_web_url ++ "repos/" ++ uName ++ "/" ++ pName ++ "/branches"
        req0 = getRequest url
        authHdr = ( S8.pack "authorization", gitstar_ssh_web_authorization)
        acceptHdr = (S8.pack "accept", acceptHdrVal)
        req  = req0 {reqHeaders = authHdr : acceptHdr : reqHeaders req0}
    privs <- appGetPolicyPriv policy
    sshResp <- liftLIO $ simpleHttpP privs req L8.empty
    if respStatusDC sshResp /= stat200
      then respondStat stat500
      else do
        body <- liftLIO $ extractBody sshResp
        let ctype = S8.unpack $ fromMaybe "text/plain" $
                      lookup "content-type" $ respHeadersDC sshResp
        render ctype body
        respondStat $ respStatusDC sshResp



-- | Get accept header, default 'application/json'
getAccept :: Monad m => Action t m S8.ByteString
getAccept = (fromMaybe "application/json") `liftM` requestHeader "x-hails-user"
