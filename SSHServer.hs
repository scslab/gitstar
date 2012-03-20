{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.State
import Data.List (isPrefixOf)
import Data.Time
import SSH.Channel
import SSH.Crypto
import SSH.Session
import System.Directory (canonicalizePath)
import System.Environment
import System.FilePath
import System.Process
import qualified Data.ByteString.Base64 as B64
import qualified SSH as SSH
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import System.Environment
import System.IO

import Policy.Gitstar (gitstar)
import Models
import Hails.Data.LBson (Binary(..))
import Hails.Database.MongoDB.Structured

import qualified LIO as LIO
import LIO.DCLabel
import DCLabel.Core (createPrivTCB)

type L = L.ByteString
type S = S.ByteString


main :: IO ()
main = do
    port    <- read `liftM` getEnv "GITSTAR_SSH_PORT" :: IO Int
    keyPath <- getEnv "GITSTAR_SSH_KEY"
    hPutStrLn stderr $ "Starting ssh server on port "++ show port  ++
                       " with key " ++ keyPath ++ " ..."
    kp <- rsaKeyPairFromFile keyPath
    --debugAddDeian 
    startSSH kp (fromIntegral port)
  where
    startSSH kp = SSH.start (sessionConfig kp) channelConfig
    sessionConfig kp = SessionConfig { scAuthMethods = ["publickey"]
                                     , scAuthorize = sshAuthorize
                                     , scKeyPair = kp }
    channelConfig = ChannelConfig { ccRequestHandler = channelRequest }

-- | Authentitcate a user.
sshAuthorize :: Authorize -> Session Bool
sshAuthorize (PublicKey name key) = liftIO $ do
  muser <- lookupUserKey key
  case muser of
    Nothing -> do putStrLn "Authentication failed."
                  return False
    Just u -> do putStrLn $ "Authenticated "++ userName u ++ "!"
                 return True
  return $ maybe False (const True) muser
sshAuthorize _ = do liftIO $ putStrLn "Expected public-key authentication."
                    return False

-- | Find a user based on their public key
lookupUserKey :: PublicKey -> IO (Maybe User)
lookupUserKey key = evalDC_ $ do
  col <- gitstar
  findBy col "users" "key" (encodeKey key)
    where evalDC_ act = fst `liftM` (evalDC act)


channelRequest :: Bool -> ChannelRequest -> Channel ()
channelRequest wr req@(Execute cmd) = do
  liftIO $ print req
channelRequest wr (Environment "LANG" _) = when wr channelSuccess
channelRequest wr r = do
  channelError $ "Unknown request: "++ show r ++ "\r\n" ++
                 "This server only accepts EXEC requests."
  when wr channelFail

-- channelRequest :: Bool -> ChannelRequest -> Channel ()
-- channelRequest wr (Execute cmd) =
--     case words cmd of
--         ["darcs", "transfer-mode", "--repodir", path] ->
--             saneRepo path darcsTransferMode
--         ["darcs", "apply", "--all", "--repodir", path] ->
--             saneRepo path darcsApply
--         ["darcs", "apply", "--all", "--debug", "--repodir", path] ->
--             saneRepo path darcsApply
--         (initialize:repoName:description) | "init" `isPrefixOf` initialize ->
--             if null repoName || not (isSane repoName)
--                 then errorWith "invalid repository name"
--                 else saneUser $ \u -> do
--                     mr <- getOwnerRepository (uName u, repoName)
--                     case mr of
--                         Nothing -> do
--                             now <- liftIO getCurrentTime
--                             newRepository Repository
--                                 { rID = Nothing
--                                 , rRev = Nothing
--                                 , rName = repoName
--                                 , rOwner = uName u
--                                 , rDescription = unwords description
--                                 , rWebsite = ""
--                                 , rCreated = now
--                                 , rForkOf = Nothing
--                                 , rMembers = []
--                                 , rIsPrivate = False
--                                 , rIssueCount = 0
--                                 }
--                             finishWith "repository created"
--                         Just _ -> errorWith "repository already exists"
--         [oblit, repoName] | "oblit" `isPrefixOf` oblit ->
--             if null repoName || not (isSane repoName)
--                 then errorWith "invalid repository name"
--                 else saneRepo repoName obliterate
--         ["scp", "-f", "--", path] ->
--             safePath path scp
--         ["scp", "-f", path] ->
--             safePath path scp
--         _ -> failWith ("invalid exec request: " ++ show cmd)
--   where
--     failWith :: String -> Channel ()
--     failWith msg = do
--         channelError msg
--         when wr channelFail
-- 
--     finishWith :: String -> Channel ()
--     finishWith msg = do
--         channelMessage msg
--         when wr channelSuccess
--         channelDone
-- 
--     errorWith :: String -> Channel ()
--     errorWith msg = do
--         channelError msg
--         when wr channelSuccess
--         channelDone
-- 
--     -- verify a path that may be two forms:
--     --
--     --     foo/         a repository "foo" owned by the current user
--     --     bar/foo/     a repository "foo" owned by user "bar";
--     --                  current user must be a member
--     saneRepo :: FilePath -> (Repository -> Channel ()) -> Channel ()
--     saneRepo p a = saneUser $ \(User { uName = un }) -> do
--         case takeWhile (not . null) . map saneName . splitDirectories $ p of
--             [ownerName, repoName] -> do
--                 mrepo <- getOwnerRepository (ownerName, repoName)
--                 case mrepo of
--                     Just r | un `elem` rMembers r -> a r
--                     _ -> errorWith "invalid repository"
--             [repoName] ->
--                 getOwnerRepository (un, repoName)
--                     >>= maybe (errorWith "invalid repository") a
--             _ -> errorWith "invalid target directory"
-- 
--     safePath :: FilePath -> (FilePath -> Channel ()) -> Channel ()
--     safePath p a = saneUser $ \(User { uName = un }) -> do
--         cp <- liftIO (canonicalizePath ("/srv/darcs/" ++ un ++ "/" ++ p))
--         case takeWhile (not . null) . splitDirectories $ cp of
--             ("/":"srv":"darcs":ownerName:repoName:_) -> do
--                 mrepo <- getOwnerRepository (ownerName, repoName)
--                 case mrepo of
--                     Just r | un `elem` (ownerName:rMembers r)->
--                         a cp
--                     _ -> errorWith "invalid path"
-- 
--             _ -> errorWith "invalid path"
-- 
--     -- verify the current user
--     saneUser :: (User -> Channel ()) -> Channel ()
--     saneUser a = do
--         mu <- gets csUser >>= getUser
--         maybe (errorWith "invalid user") a mu
-- 
--     obliterate r = execute . unwords $
--         [ "darcs"
--         , "obliterate"
--         , "--repodir"
--         , repoDir (rOwner r) (rName r)
--         ]
-- 
--     darcsTransferMode r = execute . unwords $
--         [ "darcs"
--         , "transfer-mode"
--         , "--repodir"
--         , repoDir (rOwner r) (rName r)
--         ]
-- 
--     darcsApply r = execute . unwords $
--         [ "darcs"
--         , "apply"
--         , "--all"
--         , "--repodir"
--         , repoDir (rOwner r) (rName r)
--         ]
-- 
--     scp path = execute . unwords $ ["scp", "-f", "--", path]
-- 
--     execute = spawnProcess . runInteractiveCommand
-- channelRequest wr (Environment "LANG" _) =
--     when wr channelSuccess
-- channelRequest wr r = do
--     channelError $ "this server only accepts exec requests\r\ngot: " ++ show r
--     when wr channelFail


--
-- Misc helper
--

strictify :: L -> S
strictify = S.concat . L.toChunks

lazyfy :: S -> L
lazyfy = L.pack . S.unpack

-- | Encode a public key into a 'Binary' blob
encodeKey :: PublicKey -> Binary
encodeKey key = Binary $  S.concat [t, S8.singleton ' ', B64.encode k]
  where k = strictify . blob $ key
        t = S8.pack $ case key of
              (RSAPublicKey _ _)     -> "ssh-rsa"
              (DSAPublicKey _ _ _ _) -> "ssh-dss"

-- | Inverse of 'encodeKey'
decodeKey :: Binary -> PublicKey
decodeKey (Binary bs) = blobToKey . lazyfy . B64.decodeLenient $ bs


--
-- DEBUG, REMOVE: Adding user:
--

deian = User { userName = "deian"
             , userKey  = Binary $ S8.pack "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCpuGHHMpAr3EP67oz2YcDNWqX0h3QxEnaB8Pi0rAA+GkaPv3orufqeF6/YqyP2pIlA1sGn8eAQMKMJ8QmfnmbQ6+eDva2okwJwxJJcsQK8YQM6kyk9icvmrbXrzYGgJpcquGIFGd02my7Djxi3BXooTeTwqPhnYvCJeoO1izTOuMJoP0hRntvyDPK43vUiWwbF0N8rL9lPLOOwCd8Qdh9ks+z0WRQR/qD2/RoHB4Kx3Pi06HA5M9LnhFIapKkERNmbov+JWQ5ecT/FKdYd5xA4I4lN/1DegWQVQ2sEjEbAWl05m68Va3VauQCyWolC7ticnUd+HbmOzgo3hmN52Wax"
             , userProjects = []
             }

debugAddDeian = void . evalDC $ do
  col <- gitstar
  insertRecordP priv col "users" deian
    where priv = createPrivTCB $ newPriv ("gitstar" :: Principal)
--
--
--
