{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE OverloadedStrings #-}

-- | This module exports an interface to the gitstar repo objects.
module Gitstar.Repo ( -- * Branches and refs
                      getBranches
                    , getRefs
                      -- * Blobs
                    , getBlob
                      -- * Tags
                    , getTags, getTag
                      -- * Commits
                    , getCommit
                      -- * Trees
                    , getTree
                      -- * Types
                    , module Gitstar.Repo.Types
                    ) where

import Prelude hiding (lookup)
import Policy.Gitstar
import Config

import Control.Monad

import LIO
import LIO.DCLabel

import qualified Hails.Data.LBson.Rexports.Bson as Bson
import Hails.Database.MongoDB hiding ( Action
                                     , concatMap
                                     , map
                                     , drop
                                     , isPrefixOf
                                     , length)
import Hails.Database.MongoDB.Structured

import Data.List (isPrefixOf)
import Data.IterIO.Http
import Hails.IterIO.HttpClient

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import Gitstar.Repo.Types


-- | Given user name, project name return  the branches as a list of
-- (branch-name, branch-hash) pairs
getBranches :: Repo -> DC (Maybe [(String, SHA1)])
getBranches proj = do
  mdoc   <- mkRequestToGitstarSsh (repoOwner proj)
                                  (repoName proj) "/branches"
  case mdoc of
    Just ds -> let vs = concatMap (\(_ Bson.:= (Array v)) -> v) ds
               in return $ forM vs $ \(Doc d) -> do
                    n <- Bson.lookup "name" d
                    c <- Bson.lookup "commit" d
                    s <- Bson.lookup "sha" c
                    return (n, SHA1 s)
    _ -> return Nothing

-- | Given a project and SHA1 of object return the corresponding blob
getBlob :: Repo -> SHA1 -> DC (Maybe GitBlob)
getBlob proj sha = do
  mdoc <- mkRequestToGitstarSsh (repoOwner proj)
                                (repoName proj) $ "/git/blobs/" ++ show sha
  case mdoc of
    Just doc -> return $ do
      d <- Bson.lookup "data" doc
      c <- Bson.lookup "content" d
      m <- Bson.lookup "mime_type" d
      return $ GitBlob { blobContent = S8.pack c
                       , blobMimeType = m }
    _ -> return Nothing

-- | Given a project and SHA1 of tag return the corresponding
-- tag object
getTag :: Repo -> SHA1 -> DC (Maybe GitTag)
getTag proj sha = do
  mdoc <- mkRequestToGitstarSsh (repoOwner proj)
                                (repoName proj) $ "/git/tags/" ++ show sha
  case mdoc of
    Just doc -> return $ do
      d       <- Bson.lookup "data" doc
      ptr     <- Bson.lookup "object" d >>= Bson.lookup "sha"
      tag     <- Bson.lookup "tag" d
      message <- Bson.lookup "message" d
      tagger  <- Bson.lookup "tagger" d >>= getAuthor
      return GitTag { tagPtr       = SHA1 ptr
                    , tagName      = S8.pack tag
                    , tagMessage   = S8.pack message
                    , tagAuthor    = tagger }
    _ -> return Nothing

-- | Consturct an 'Author' from 'BsonDocument'
getAuthor :: Monad m => Bson.Document -> m Author
getAuthor d = do
  aDate  <- Bson.lookup "date"  d
  aName  <- Bson.lookup "name"  d
  aEmail <- Bson.lookup "email" d
  return Author { authDate  = S8.pack aDate
                , authName  = S8.pack aName
                , authEmail = S8.pack aEmail }

-- | Given user name, project name return the tags as a list of
-- (tag-name, branch-hash) pairs
getTags :: Repo -> DC (Maybe [(String, SHA1)])
getTags proj = do
  mdoc   <- mkRequestToGitstarSsh (repoOwner proj) (repoName proj) "/tags"
  case mdoc of
    Just ds -> let vs = concatMap (\(_ Bson.:= Array v) -> v) ds
               in return $ forM vs $ \(Doc d) -> do
                    n <- Bson.lookup "name" d
                    c <- Bson.lookup "commit" d
                    s <- Bson.lookup "sha" c
                    return (n, SHA1 s)
    _ -> return Nothing

-- | Given a project and SHA1 of commit return the corresponding
-- commit object
getCommit :: Repo -> SHA1 -> DC (Maybe GitCommit)
getCommit proj sha = do
  mdoc <- mkRequestToGitstarSsh (repoOwner proj)
                                (repoName proj) $ "/git/commits/" ++ show sha
  case mdoc of
    Just doc -> return $ do
      d         <- Bson.lookup "data" doc
      ptr       <- Bson.lookup "sha" d
      author    <- Bson.lookup "author" d >>= getAuthor
      committer <- Bson.lookup "committer" d >>= getAuthor
      message   <- Bson.lookup "message" d
      ps        <- Bson.lookup "parents" d
      parents   <- mapM (Bson.lookup "sha") ps
      tree      <- Bson.lookup "tree" d >>= Bson.lookup "sha"
      return GitCommit { cmtPtr       = SHA1 ptr
                       , cmtAuthor    = author
                       , cmtCommitter = committer
                       , cmtTree      = SHA1 tree
                       , cmtMessage   = S8.pack message
                       , cmtParents   = map SHA1 parents}
    _ -> return Nothing

-- | Given a project and SHA1 of tree return the corresponding
-- tree object
getTree :: Repo -> SHA1 -> DC (Maybe GitTree)
getTree proj sha = do
  mdoc <- mkRequestToGitstarSsh (repoOwner proj)
                                (repoName proj) $ "/git/trees/" ++ show sha
  case mdoc of
    Just doc -> return $ do d  <- Bson.lookup "data" doc
                            ps <- Bson.lookup "tree" d
                            mapM mkTreeOrBlob ps
    _ -> return Nothing

-- | Given a document read the corresponding tree or blob tree entry.
mkTreeOrBlob :: Bson.Document -> Maybe GitTreeEntry
mkTreeOrBlob d = do
  tPath <- Bson.lookup "path" d
  tMode <- Bson.lookup "mode" d
  tType <- Bson.lookup "type" d
  let tSize = Bson.lookup "type" d
  tPtr  <- Bson.lookup "sha"  d
  return GitTreeEntry { entPath = tPath
                      , entMode = readGitMode tMode
                      , entType = readGitType tType
                      , entSize = tSize
                      , entPtr  = SHA1 tPtr }

-- | Read git object type
readGitType :: String -> GitType
readGitType s | s == "blob"   = GtBlob
              | s == "tree"   = GtTree
              | s == "commit" = GtCommit
              | s == "tag"    = GtTag
              | otherwise     = error $ "readGitType: unknown " ++ s

-- | Read git object mode
readGitMode :: String -> GitMode
readGitMode s | s == "040000"        = Directory              
              | s == "120000"        = SymbolicLink
              | s == "160000"        = GitLink  
              | "100" `isPrefixOf` s = RegularFile (read $ drop 3 s)
              | otherwise            = error $ "readGitMode: unknown " ++ s
  

-- | Given user name, project name return the refs
getRefs :: Repo -> Url -> DC (Maybe [GitRef])
getRefs proj suffix = do
  mdoc   <- mkRequestToGitstarSsh (repoOwner proj) 
                                  (repoName proj) $ "/git/refs" ++ suffix
  case mdoc of
    Just doc -> return $ do rs  <- Bson.lookup "data" doc
                            mapM mkRef rs
    _ -> return Nothing

--  | Guiven a document read the corresponding ref
mkRef :: Bson.Document -> Maybe GitRef
mkRef d = do
  rName <- Bson.lookup "ref" d
  o     <- Bson.lookup "object" d
  rType <- Bson.lookup "type" o
  rPtr  <- Bson.lookup "sha" o
  return GitRef { refName = rName
                , refType = readGitType rType
                , refPtr  = SHA1 rPtr }


--
-- Helpers
--

-- | Given user name, project name and URL suffix make GET
-- request to gitstar-ssh-web server
-- The request made will be: @GET /repos/usr/proj/urlSuffix@
mkRequestToGitstarSsh :: UserName
                      -> ProjectName
                      -> Url 
                      -> DC (Maybe BsonDocument)
mkRequestToGitstarSsh usr proj urlSuffix = do
  policy <- liftLIO gitstar
    -- Make sure current user can read:
  mProj  <- findWhere policy $ select [ "name"  =: proj
                                      , "owner" =: usr ] "projects"
  case mProj of
    Nothing -> return Nothing
    Just Project{} -> do
       let url = gitstar_ssh_web_url ++ "repos/" ++ usr ++ "/"
                                     ++ proj ++ urlSuffix
           req0 = getRequest url
           authHdr = (S8.pack "authorization", gitstar_ssh_web_authorization)
           acceptHdr = (S8.pack "accept", S8.pack "application/bson")
           req  = req0 {reqHeaders = authHdr : acceptHdr : reqHeaders req0}
       sshResp <- simpleHttp req L8.empty
       if respStatusDC sshResp /= stat200
         then return Nothing
         else do
           body <- liftLIO $ extractBody sshResp
           return . Just . decodeDoc $ body
