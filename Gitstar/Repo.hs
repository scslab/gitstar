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
                      -- * Diffs
                    , getDiffs
                      -- * Stats
                    , getStats
                      -- * Blame
                    , getBlame
                      -- * Types
                    , module Gitstar.Repo.Types
                    ) where

import Prelude hiding (lookup)
import Policy.Gitstar

import Control.Monad

import LIO.DCLabel

import qualified Hails.Data.LBson.Rexports.Bson as Bson
import Hails.Database.MongoDB hiding ( Action
                                     , concatMap
                                     , map
                                     , drop
                                     , isPrefixOf
                                     , length)

import Data.List (isPrefixOf)

import qualified Data.ByteString.Char8 as S8

import Gitstar.Repo.Types


-- | Given a repo return the branches as a list of
-- (branch-name, branch-sha) pairs
getBranches :: Repo -> DC (Maybe [(String, SHA1)])
getBranches proj = do
  mdoc   <- gitstarRepoHttp (repoOwner proj)
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
  mdoc <- gitstarRepoHttp (repoOwner proj)
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
  mdoc <- gitstarRepoHttp (repoOwner proj)
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
  mdoc   <- gitstarRepoHttp (repoOwner proj) (repoName proj) "/tags"
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
  mdoc <- gitstarRepoHttp (repoOwner proj)
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
  mdoc <- gitstarRepoHttp (repoOwner proj)
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
  let tSize = Bson.lookup "size" d
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
  mdoc   <- gitstarRepoHttp (repoOwner proj) 
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


-- | Get all the file changes for a repo and commit id
getDiffs :: Repo -> SHA1 -> DC (Maybe [GitDiff])
getDiffs proj sha = do
  mdoc   <- gitstarRepoHttp (repoOwner proj) 
                            (repoName proj) $ "/git/commits/" ++ show sha
                                                              ++ "/diff"
  case mdoc of
    Just doc -> return $ do
      ds <- Bson.lookup "data" doc
      forM ds $ \d -> do
           path <- Bson.lookup  "path" d
           newF <- Bson.lookup "new_file" d
           delF <- Bson.lookup "deleted_file" d
           let pathXfm = case () of
                          _ | newF -> Just NewFile
                          _ | delF -> Just DeletedFile
                          _        -> Nothing
           sIdx  <- Bson.lookup "similarity_index" d
           diff  <- Bson.lookup "diff" d
           return GitDiff { diffPath = DiffPath
                               { dpathName = path
                               , dpathChanges = pathXfm }
                          , diffSimilarlityIndex = sIdx
                          , diffContent = S8.pack diff }
    _ -> return Nothing




-- | Given a repo and commit id,  return the corresponding stats
getStats :: Repo -> SHA1 -> DC (Maybe GitStats)
getStats proj sha = do
  mdoc   <- gitstarRepoHttp (repoOwner proj)
                            (repoName proj) $ "/git/commits/" ++ show sha
                                                              ++ "/stats"
  case mdoc of
    Just doc -> return $ do
      d         <- Bson.lookup "data" doc
      cmt       <- Bson.lookup "sha" d
      files     <- Bson.lookup "files" d
      fstats    <- mapM docToFileStat files
      adds      <- Bson.lookup "additions" d
      dels      <- Bson.lookup "deletions" d
      tots      <- Bson.lookup "total" d
      return GitStats { statCommit    = SHA1 cmt
                      , statFiles     = fstats
                      , statAdditions = adds
                      , statDeletions = dels
                      , statTotal     = tots }
    _ -> return Nothing
   where docToFileStat d = do
           file <- Bson.lookup "file" d
           adds <- Bson.lookup "additions" d
           dels <- Bson.lookup "deletions" d
           tots <- Bson.lookup "total" d
           return GitFileStat { fstatPath = file
                              , fstatAdditions = adds
                              , fstatDeletions = dels
                              , fstatTotal     = tots }

-- | Return the blame for all the lines in a file.
getBlame :: Repo -> SHA1 -> FilePath -> DC (Maybe GitBlame)
getBlame proj sha file = do
  mdoc   <- gitstarRepoHttp (repoOwner proj)
                            (repoName proj) $ "/git/blame/" ++ show sha
                                                            ++ "/" ++ file
  case mdoc of
    Just doc -> return $ do
      ds <- Bson.lookup "data" doc
      forM ds $ \d -> do
        no   <- Bson.lookup "lineno" d
        oNo  <- Bson.lookup "old_lineno" d
        line <- Bson.lookup "line" d
        cmt  <- Bson.lookup "commit" d
        return GitBlameLine { blmLineNr    = no
                            , blmOldLineNr = oNo
                            , blmLine      = S8.pack line
                            , blmCommit    = SHA1 cmt }
    _ -> return Nothing
