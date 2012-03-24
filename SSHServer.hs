{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (lookup)
import Control.Monad.State

import SSH.Channel
import SSH.Crypto
import SSH.Session
import qualified SSH

import System.Environment
import System.FilePath
import System.Process

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromJust, isJust)

import Text.Regex.Posix

import System.IO

import Policy.Gitstar
import Models

import Hails.Data.LBson ( Document
                        , Binary(..) 
                        , genObjectId
                        , (=:)
                        )
import Hails.Database.MongoDB (select)

import Data.IORef

import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpClient

import LIO.DCLabel (DCLabel)
{-
import LIO (unlabelP)
import DCLabel.Core (createPrivTCB)

import LIO.TCB
-}

import qualified Data.ByteString.Lazy.Char8 as L8
import Hails.Data.LBson ( encodeDoc, decodeDoc
                        , lookup
                        , safeToBsonDoc, safeFromBsonDoc)

type L = L.ByteString
type S = S.ByteString

-- #define DEBUG

main :: IO ()
main = do
    port    <- read `liftM` getEnv "GITSTAR_SSH_PORT" :: IO Int
    keyPath <- getEnv "GITSTAR_SSH_KEY"
    hPutStrLn stderr $ "Starting ssh server on port "++ show port  ++
                       " with key " ++ keyPath ++ " ..."
    kp <- rsaKeyPairFromFile keyPath
#ifdef DEBUG
    debugAddUserAndProjs 
#else
    startSSH kp (fromIntegral port)
#endif
  where
    startSSH kp = SSH.start (sessionConfig kp) channelConfig
    sessionConfig kp = SessionConfig { scAuthMethods = ["publickey"]
                                     , scAuthorize = sshAuthorize
                                     , scKeyPair = kp }
    channelConfig = ChannelConfig { ccRequestHandler = channelRequest }

-- | Handle a request. Based on the example it seems like the @wr@
-- value is used a write-back flag.
channelRequest :: Bool -> ChannelRequest -> Channel ()
channelRequest wr (Execute cmd) = handleExecRequest wr cmd
channelRequest wr (Environment "LANG" _) = when wr channelSuccess
channelRequest wr r = do
  channelError $ "Unknown request: "++ show r ++ "\r\n" ++
                 "This server only accepts EXEC requests."
  when wr channelFail

--
-- Handle git-(upload|receive)-pack
--

-- | Top-level exec handler
handleExecRequest :: Bool -> String -> Channel ()
handleExecRequest wr cmd = do
  liftIO $ hPutStrLn stderr $ "Handling: " ++ cmd
  case words cmd of
    ["git-receive-pack", path] -> handleReceivePack wr path
    ("git-upload-pack":opts)   -> handleUploadPack wr opts
    _ -> errorWith wr $ "Invalid exec request: " ++ cmd

-- | Handle git-receive-pack
handleReceivePack :: Bool -> FilePath -> Channel ()
handleReceivePack wr path = do
  mRepoInfo <- pathToRepoInfo wr path
  case mRepoInfo of
    Nothing -> return ()
    Just (owner, repo) -> do
      rootPath <- liftIO $ getEnv "GITSTAR_ROOT_PATH"
      let fullPath = rootPath </>  owner </> repo
      ok <- checkWriteAccess owner repo
      if ok
        then execute . unwords $ ["git-receive-pack", fullPath]
        else errorWith wr $ "Insufficient access."


-- | Handle git-upload-pack
handleUploadPack :: Bool -> [String] -> Channel ()
handleUploadPack wr opts = do
  unless validOptions failBadOpts
  mRepoInfo <- pathToRepoInfo wr path
  when (validOptions && isJust mRepoInfo) $ do
    let (owner, repo) = fromJust mRepoInfo
    rootPath <- liftIO $ getEnv "GITSTAR_ROOT_PATH"
    let fullPath = rootPath </>  owner </> repo
    ok <- checkReadAccess owner repo
    if ok
      then execute . unwords $ ["git-upload-pack", newOpts, fullPath]
      else errorWith wr $ "Insufficient access."
  where path = safeLast opts
        -- Extract opts:
        newOpts = unwords $
          let haveStrict =  strict `elem` opts
              opts' = splitTout . filter (/= strict) $ safeInit opts
          in concat $
               [ if haveStrict then [strict] else []
               , case opts' of
                   ["--timeout=",tval] | tval =~ ("[0-9]+" :: String) ->
                        [tout ++ tval]
                   _ -> []
               ]
        --
        splitTout []     = []
        splitTout (o:os) = if o =~ (tout++"[0-9]+" :: String)
                             then tout : ( drop (length tout) o : os)
                             else o : os
        -- Make sure opts are of the form: [--strict] [--timeout=<n>]
        validOptions =
          let o = unwords $ safeInit opts
              re0 = "--strict(\\s+--timeout=\\s*[0-9]+)?"
              re1 = "--timeout=\\s*[0-9]+(\\s+--strict)?"
              re  = "^(|" ++ re0 ++ "|" ++ re1 ++")$" 
          in (o =~ (re :: String)) :: Bool
        --
        tout = "--timeout="
        --
        strict = "--strict"
        --
        safeLast xs | xs == []  = []
                    | otherwise = last xs
        --
        safeInit xs | length xs <= 1 = []
                    | otherwise      = init xs
        --
        failBadOpts = errorWith wr $ "Invalid options. Expected: " 
                  ++  "git-upload-pack [--strict] [--timeout=<n>] /user/repo"

--
-- Handle authentication and access control
--

-- | Very current user has read access to @/owner/pName@.
checkReadAccess :: UserName -> String -> Channel Bool
checkReadAccess owner pName = do
  usr <- gets csUser
  mp <- liftIO $ getProject owner pName
  liftIO $ print mp
  return $ maybe False (isReader usr) mp
    where isReader usr p = case projectReaders p of
            Left Public -> True
            Right rs -> usr `elem` ((projectOwner p : projectCollaborators p)
                                    ++ rs)

-- | Very current user has write access to @/owner/pName@.
checkWriteAccess :: UserName -> String -> Channel Bool
checkWriteAccess owner pName = do
  usr <- gets csUser
  mp <- liftIO $ getProject owner pName
  liftIO $ print mp
  return $ maybe False (isWriter usr) mp
    where isWriter usr p = usr `elem` (projectOwner p : projectCollaborators p)

-- | Find a project given the owner and repo (project name).
getProject :: UserName -> String -> IO (Maybe Project)
getProject owner project = do
  let req0 = (getRequest $ "http://gitstar.lvh.me:5000/"
                              ++ owner ++ "/" ++ project)
      authHdr = ("Authorization", "gitstar:w00t")
      accHdr  = ("Accept", "application/bson")
      req = req0 { reqHeaders = authHdr : (accHdr : reqHeaders req0) }
  resp <- genSimpleHttp req L.empty Nothing 0 False
  if respStatus resp /= stat200
    then return Nothing
    else fromDocument `liftM` bsonDocFromBody resp

-- | Construct Bson document from response body.
bsonDocFromBody :: HttpResp IO -> IO (Document DCLabel)
bsonDocFromBody resp = do
  ref <- newIORef []
  (respBody resp |. maybeChunk) |$ (docI ref)
  readIORef ref
    where maybeChunk = if respChunk resp then inumToChunks else inumNop
          docI ref = do x <- pureI
                        liftIO $ writeIORef ref (decodeDoc x)

-- | Authentitcate a user.
sshAuthorize :: Authorize -> Session Bool
sshAuthorize (PublicKey uName key) = liftIO $ do
  hPutStr stderr $ "Authenticating " ++ uName ++ "..."
  verifyOk <- verifyUserKey uName key
  if verifyOk 
    then hPutStrLn stderr "OK!"
    else hPutStrLn stderr "FAILED!"
  return verifyOk
sshAuthorize _ = do
  liftIO $ hPutStrLn stderr "Expected public-key authentication."
  return False

-- | Find a user based and verify their public key.
verifyUserKey :: String -> PublicKey -> IO Bool
verifyUserKey uName key = do
  mkeys <- getUserKeys uName
  return $ case mkeys of 
    Nothing -> False
    Just keys -> let kVal = encodeKey key
                 in any ((== kVal) . sshKeyValue) keys

-- | Get all the user keys.
getUserKeys :: UserName -> IO (Maybe [SSHKey])
getUserKeys uName = do
  let req0 = (getRequest $ "http://gitstar.lvh.me:5000/"
                              ++ uName ++ "/keys")
      authHdr = ("Authorization", "gitstar:w00t")
      accHdr  = ("Accept", "application/bson")
      req = req0 { reqHeaders = authHdr : (accHdr : reqHeaders req0) }
  resp <- genSimpleHttp req L.empty Nothing 0 False
  if respStatus resp /= stat200
    then return Nothing
    else do doc <- bsonDocFromBody resp
            let mkeyDocs = do
                 v <- lookup "keys" doc
                 mapM safeFromBsonDoc v
            return $ case mkeyDocs of
              Nothing -> Nothing
              Just keyDocs -> mapM fromDocument keyDocs

--
-- Misc helper
--

execute :: String -> Channel ()
execute = spawnProcess . runInteractiveCommand

strictify :: L -> S
strictify = S.concat . L.toChunks

lazyfy :: S -> L
lazyfy = L.pack . S.unpack

-- | Encode a public key into a 'Binary' blob
encodeKey :: PublicKey -> Binary
encodeKey key = Binary $  S.concat [t, S8.singleton ' ', B64.encode k]
  where k = strictify . blob $ key
        t = S8.pack $ case key of
              (RSAPublicKey {}) -> "ssh-rsa"
              (DSAPublicKey {}) -> "ssh-dss"

-- | Inverse of 'encodeKey'
decodeKey :: Binary -> PublicKey
decodeKey (Binary bs) = blobToKey . lazyfy . B64.decodeLenient $ bs

-- | Strip enclosing quotes.
stripQuotes :: String -> String
stripQuotes = lstrip . rstrip
  where lstrip s = case s of
          [] -> []
          (x:xs) -> if x == '\'' then xs else s
        rstrip = reverse . lstrip . reverse

-- | Print error to channel and finish.
errorWith :: Bool -> String -> Channel ()
errorWith wr msg = do
    liftIO $ hPutStrLn stderr msg
    channelError msg
    when wr channelSuccess
    channelDone

-- | Convert a path (e.g., @/user/repo.git@) to the corresponding
-- owner (@user@) and repository (@repo.git@)
pathToRepoInfo :: Bool -> FilePath -> Channel (Maybe (UserName, FilePath))
pathToRepoInfo wr path = 
  case splitDirectories (stripQuotes path) of
    ["/",user,repo] -> if malformed user || malformed repo
                        then failBadPath else return $ Just (user, repo)
    _ -> failBadPath
  where failBadPath = do errorWith wr "Invalid path. Expected form: /user/repo"
                         return Nothing
        malformed s = s =~ ("\\.\\." :: String)


#ifdef DEBUG
--
-- DEBUG, REMOVE: Adding user:
--

deian kid = User { userName = "deian"
                 , userKeys = [SSHKey { sshKeyId = kid
                                      , sshKeyTitle = "stmarks"
                                      , sshKeyValue = Binary $ S8.pack
                                      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCpuGHHMpAr3EP67oz2YcDNWqX0h3QxEnaB8Pi0rAA+GkaPv3orufqeF6/YqyP2pIlA1sGn8eAQMKMJ8QmfnmbQ6+eDva2okwJwxJJcsQK8YQM6kyk9icvmrbXrzYGgJpcquGIFGd02my7Djxi3BXooTeTwqPhnYvCJeoO1izTOuMJoP0hRntvyDPK43vUiWwbF0N8rL9lPLOOwCd8Qdh9ks+z0WRQR/qD2/RoHB4Kx3Pi06HA5M9LnhFIapKkERNmbov+JWQ5ecT/FKdYd5xA4I4lN/1DegWQVQ2sEjEbAWl05m68Va3VauQCyWolC7ticnUd+HbmOzgo3hmN52Wax"}]
             , userProjects = []
             }

proj0 :: Project
proj0 = Project { projectId = Nothing
                , projectName = "proj0"
                , projectOwner = "deian"
                , projectDescription = "desc"
                , projectCollaborators = ["amit", "david"]
                , projectReaders = Right [] }

proj1 :: Project
proj1 = Project { projectId = Nothing
                , projectName = "proj1"
                , projectOwner = "amit"
                , projectDescription = "desc"
                , projectCollaborators = ["deian", "david"]
                , projectReaders = Right [] }

proj2 :: Project
proj2 = Project { projectId = Nothing
                , projectName = "proj2"
                , projectOwner = "amit"
                , projectDescription = "desc"
                , projectCollaborators = []
                , projectReaders = Left Public }

proj3 :: Project
proj3 = Project { projectId = Nothing
                , projectName = "proj3"
                , projectOwner = "amit"
                , projectDescription = "desc"
                , projectCollaborators = []
                , projectReaders = Right ["deian"] }
proj4 :: Project
proj4 = Project { projectId = Nothing
                , projectName = "proj4"
                , projectOwner = "amit"
                , projectDescription = "desc"
                , projectCollaborators = []
                , projectReaders = Right [] }


debugAddUserAndProjs :: IO ()
debugAddUserAndProjs = void . evalDC $ do
  col <- gitstar
  kid <- genObjectId
  void $ insertRecordP p col (deian kid)
  forM_ [proj0,proj1,proj2,proj3,proj4] $ \proj -> do
    --insertRecordP p col proj
    let encD  = encodeDoc $ toDocument proj
    ioTCB . print $ encD
    where p = createPrivTCB $ newPriv ("_gitstar" :: Principal)
--
--
--
#endif
