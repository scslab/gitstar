{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controllers.Repos ( repoShowBranches
                         , repoShowTags
                         , repoShowGitTag
                         , repoShowGitBlob
                         , repoShowGitCommit
                         , repoShowGitTree
                         , repoShowGitRefs
                         ) where

import Policy.Gitstar
import Config
import Utils

import Data.Maybe
import Control.Monad

import LIO
import LIO.DCLabel

import Hails.Database.MongoDB (select, (=:))
import Hails.Database.MongoDB.Structured

import Data.List (isPrefixOf, isInfixOf)
import Data.IterIO.Http
import Data.IterIO.Http.Support
import Hails.IterIO.HttpClient

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

-- /repos/:user_name/:project_name/branches
repoShowBranches :: Action t b DC ()
repoShowBranches = mkRequestToGitstarSsh "/branches"

-- /repos/:user_name/:project_name/tags
repoShowTags :: Action t b DC ()
repoShowTags = mkRequestToGitstarSsh "/tags"

-- /repos/:user_name/:project_name/git/tags/:id
repoShowGitTag :: Action t b DC ()
repoShowGitTag = do
  sha  <- getParamVal "id"
  mkRequestToGitstarSsh $ "/git/tags/" ++ sha

-- /repos/:user_name/:project_name/git/blobs/:id
repoShowGitBlob :: Action t b DC ()
repoShowGitBlob = do
  sha  <- getParamVal "id"
  mkRequestToGitstarSsh $ "/git/blobs/" ++ sha

-- /repos/:user_name/:project_name/git/commits/:id
repoShowGitCommit :: Action t b DC ()
repoShowGitCommit = do
  sha  <- getParamVal "id"
  mkRequestToGitstarSsh $ "/git/commits/" ++ sha

-- /repos/:user_name/:project_name/git/trees/:id
repoShowGitTree :: Action t b DC ()
repoShowGitTree = do
  sha  <- getParamVal "id"
  mkRequestToGitstarSsh $ "/git/trees/" ++ sha

-- /repos/:user_name/:project_name/git/refs
repoShowGitRefs :: Action t b DC ()
repoShowGitRefs = do
  req <- getHttpReq
  uName  <- getParamVal "user_name"
  pName  <- getParamVal "project_name"
  let prefix = "/repos/" ++ uName ++ "/" ++ pName
      path = S8.unpack . reqPath $ req
  if ".." `isInfixOf` path || (not $ prefix `isPrefixOf` path)
    then respond404 -- respondStat stat403
    else mkRequestToGitstarSsh $ drop (length prefix) path


--
-- Helpers
--

-- | Given user name, project name and URL suffix make GET
-- request to gitstar-ssh-web server
-- The request made will be: @GET /repos/user_name/project_name/urlSuffix@
mkRequestToGitstarSsh :: String -> Action t b DC ()
mkRequestToGitstarSsh urlSuffix = do
  policy <- liftLIO gitstar
  uName  <- getParamVal "user_name"
  pName  <- getParamVal "project_name"
  mProj  <- liftLIO $
    -- Make sure current user can read:
    findWhere policy $ select [ "name"  =: pName, "owner" =: uName ] "projects"
  with404orJust mProj $ \(_ :: Project) -> do
    acceptHdrVal <- getAccept
    let url = gitstar_ssh_web_url ++ "repos/" ++ uName ++ "/"
                                  ++ pName ++ urlSuffix
        req0 = getRequest url
        authHdr = (S8.pack "authorization", gitstar_ssh_web_authorization)
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
  where -- | Get accept header, default 'application/json'
        getAccept = fromMaybe "application/json" `liftM`
                                  requestHeader "x-hails-user"
