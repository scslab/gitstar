{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, IncoherentInstances #-}
module Policy.Gitstar ( gitstar
                      , GitstarPolicy
                      -- * Projects
                      , ProjectId, Project(..), Public(..)
                      -- * Users
                      , UserName, User(..), SSHKey(..)
                      , getOrCreateUser
                      , partialUserUpdate 
                      , addUserKey 
                      ) where

import Prelude hiding (lookup)

import Control.Monad

import Data.Maybe (listToMaybe, fromJust)
import Data.Typeable
import Hails.Data.LBson hiding (map, head, tail, words, key)

import Hails.App
import Hails.Database
import Hails.Database.MongoDB hiding (Action, map, head, tail, words, key)
import Hails.Database.MongoDB.Structured

import LIO.MonadCatch

import qualified Data.ByteString.Char8 as S8

-- | Policy handler
gitstar :: DC GitstarPolicy
gitstar = mkPolicy

data GitstarPolicy = GitstarPolicy TCBPriv (Database DCLabel)
  deriving (Typeable)

instance DatabasePolicy GitstarPolicy where
  createDatabasePolicy conf p = do
    db <- labelDatabase conf lcollections lpub
    db' <- foldM (\d col -> do
              c <- col p
              assocCollectionP p c d) db [ projectsCollection
                                         , usersCollection
                                         ]
    return $ GitstarPolicy p db'
      where lcollections = newDC (<>) (owner p)

  policyDB (GitstarPolicy _ db) = db

  policyOwner (GitstarPolicy p _) = principal . owner $ p

instance PrivilegeGrantGate GitstarPolicy where
  grantPriv policy@(GitstarPolicy p _) app = getLabel >>= \curL ->
     let l = newDC (secrecy curL) (policyOwner policy)
     in mkGateP p l analyze
        where analyze desc =
                if app == "gitstar" && desc `canDelegate` newPriv app
                  then p
                  else noPrivs
                      

    

-- | Extract the principal of a DCLabel singleton component.
extractPrincipal :: Component -> Maybe Principal
extractPrincipal c | c == (><) = Nothing
                   | otherwise =  case componentToList c of
                                    [MkDisj [p]] -> Just p
                                    _ -> Nothing

-- | Get the only principal that owns the privileges.
-- Note that this will result in an error if the privilege is 
-- not a list of one principal
owner :: DCPrivTCB -> String
owner = S8.unpack . name . fromJust . extractPrincipal . priv

--
-- Key model
--

-- | A key id is an object ID
type KeyId = ObjectId

-- | An SSH key has a name and key value
data SSHKey = SSHKey { sshKeyId    :: KeyId    -- ^ Key id
                     , sshKeyTitle :: !String  -- ^ Name
                     , sshKeyValue :: !Binary  -- ^ Actual key
                     } deriving (Show, Eq)

instance DCRecord SSHKey where
  fromDocument doc = do
    i <- lookup (u "_id") doc
    t <- lookup (u "title") doc
    v <- lookup (u "value")  doc
    return SSHKey { sshKeyId = i
                  , sshKeyTitle = t
                  , sshKeyValue = v }
  toDocument k = [ (u "_id")   =: sshKeyId k
                 , (u "title") =: sshKeyTitle k
                 , (u "value") =: sshKeyValue k ]
  collectionName = error "Not insertable"

--
-- User model
--

-- | Collection keeping track of users
-- /Security properties:/
--
--   * User name and ssh-key are searchable
--
--   * Only gitstar or user may modify the ssh key and project list
--
usersCollection :: TCBPriv -> DC (Collection DCLabel)
usersCollection p = collectionP p "users" lpub colClearance $
  RawPolicy (userLabel . fromJust . fromDocument)
            [ ("_id", SearchableField)
            , ("key", SearchableField)
            ]
   where userLabel usr = newDC (<>) ((userName usr) .\/. (owner p))
         colClearance = newDC (owner p) (<>)



-- | User name is simply  a stirng
type UserName = String

-- | Email address of a user
type Email = String

-- | URL
type Url = String

-- | Data type describing users
data User = User { userName     :: UserName     -- ^ Username
                 , userKeys     :: [SSHKey]     -- ^ User's ssh keys
                 , userProjects :: [ProjectId]  -- ^ User's projects
                 , userFullName :: Maybe String -- ^ User's full name
                 , userCity     :: Maybe String -- ^ User's location
                 , userWebsite  :: Maybe Url    -- ^ User's website
                 , userGravatar :: Maybe Email  -- ^ User's gravatar e-mail
                 } deriving (Show, Eq)

instance DCRecord User where
  fromDocument doc = do
    uName   <- lookup (u "_id") doc
    keyDocs <- lookup (u "keys") doc
    keys <- case mapM safeFromBsonDoc keyDocs of
               Nothing -> fail "fromDocument: safeFromBsonDoc failed"
               Just ks -> mapM fromDocument ks
    uPrjs <- lookup (u "projects") doc
    return $ User { userName      = uName
                  , userKeys      = keys
                  , userProjects  = uPrjs
                  , userFullName = lookup (u "full_name") doc
                  , userCity = lookup (u "city") doc
                  , userWebsite = lookup (u "website") doc
                  , userGravatar = lookup (u "gravatar") doc}

  toDocument usr = [ (u "_id")       =: userName usr
                   , (u "keys")      =: (map sshKeyToDoc $ userKeys usr)
                   , (u "projects")  =: userProjects usr
                   , (u "full_name") =: userFullName usr
                   , (u "city")      =: userCity usr
                   , (u "website")   =: userWebsite usr
                   , (u "gravatar")  =: userGravatar usr]
    where sshKeyToDoc = (fromJust . safeToBsonDoc . toDocument)
  collectionName _ = "users"

instance DCLabeledRecord User where

-- | Get the user and if it's not already in the DB, insert it.
getOrCreateUser :: UserName -> DC User
getOrCreateUser uName = do
  policy@(GitstarPolicy privs _) <- gitstar
  mres   <- findBy policy  "users" "_id" uName
  case mres of
    Just usr -> return usr
    _ -> do res <- insertRecordP privs policy newUser
            either (fail "Failed to create user") (const $ return newUser) res
    where newUser = User { userName = uName
                         , userKeys = []
                         , userProjects = []
                         , userFullName = Nothing
                         , userCity = Nothing
                         , userWebsite = Nothing
                         , userGravatar = Nothing
                         }

-- | Execute a \"toLabeled\" gate with gitstar prvileges
gitstarSafeToLabeled :: DCLabeled (Document DCLabel) 
                     -> (Document DCLabel -> DC a) -> DC (DCLabeled a)
gitstarSafeToLabeled ldoc act = do
  (GitstarPolicy privs _) <- gitstar
  gateToLabeled privs ldoc act

-- | Given a user name and partial document for a 'User', return a
-- labeld user (endorsed by the policy). The projects and actual user
-- id are not modified if present in the document.
partialUserUpdate :: UserName
                  -> DCLabeled (Document DCLabel) 
                  -> DC (DCLabeled User)
partialUserUpdate username ldoc = do
  user <- getOrCreateUser username
  gitstarSafeToLabeled ldoc $ \partialDoc -> 
        -- Do not touch the user name and projects:
    let doc0 = (exclude ["projects", "_id"] partialDoc)
        doc1 = toDocument user
    in fromDocument $ merge doc0 doc1 -- create new user


-- | Given a username and a labeled document corresponding to a key,
-- find the user in the DB and return a 'User' value with the key
-- added. The resultant value is endorsed by the policy/service.
addUserKey :: UserName -> DCLabeled (Document DCLabel) -> DC (DCLabeled User)
addUserKey username ldoc = do
  user <- getOrCreateUser username
  gitstarSafeToLabeled ldoc $ \doc -> do
    -- generate a new key id:
    newId <- genObjectId
    -- The key value is expected to be a string, which we convert to
    -- 'Binary' to match types
    v <- mkKeyValueBinary doc 
    -- create key object:
    key <- fromDocument $ merge ["_id" =: newId, "value" =: v] doc
    -- Return the new user:
    return $ user { userKeys = key : userKeys user }
      where mkKeyValueBinary doc = 
              (Binary . S8.pack) `liftM` lookup (u "value")  doc


--
-- Projects model
--


-- | Collection keeping track of projects
-- /Security properties:/
--
--   * Project id, name and owner are searchable
--
--   * If the project is not public, only collaborators and readers
--     (and gitstar) may read the description and repository data
--
--   * Only gitstar and collaborators may write to the repository
--
--   * Only gitstar and owner may modify document
--
projectsCollection :: TCBPriv -> DC (Collection DCLabel)
projectsCollection p = collectionP p "projects" lpub colClearance $
  RawPolicy (labelForProject . fromJust . fromDocument)
            [ ("_id",   SearchableField)
            , ("name",  SearchableField)
            , ("owner", SearchableField)
            ]
    where colClearance = newDC (owner p) (<>)
          labelForProject proj = 
            let collabs = projectCollaborators proj
                r = case projectReaders proj of
                      Left Public -> (<>)
                      Right rs -> listToComponent [listToDisj $
                                    (projectOwner proj):(rs ++ collabs)]
            in newDC (owner p .\/. r)
                     ((projectOwner proj) .\/.  owner p)

-- | Data type denoting public projects
data Public = Public
  deriving (Show, Read)

-- | Project id is simply an object id
type ProjectId = Maybe ObjectId

-- | Project name is simply a stirng
type ProjectName = String

-- | A data type describing a project
data Project = Project {
    projectId            :: ProjectId
    -- ^ Project id
  , projectName          :: ProjectName
    -- ^ Project name
  , projectOwner         :: UserName
    -- ^ Project owner
  , projectDescription   :: String
    -- ^ Project descritption
  , projectCollaborators :: [UserName]
    -- ^ Project collaborators that can read and write to repository
  , projectReaders       :: Either Public [UserName]
    -- ^ Project is either public or private to the readers and
    -- collaborators
  } deriving (Show)

instance DCRecord Project where
  fromDocument doc = do
    pName  <- lookup (u "name") doc
    pOwner <- lookup (u "owner") doc
    pDesc  <- lookup (u "description") doc
    pColls <- lookup (u "collaborators") doc
    pRedrs <- lookup (u "readers") doc

    return $ Project
      { projectId            = lookup (u "_id") doc
      , projectName          = pName 
      , projectOwner         = pOwner
      , projectDescription   = pDesc 
      , projectCollaborators = pColls
      , projectReaders       = readersDocToEither pRedrs }

  toDocument proj =
    (maybe [] (\i -> [(u "_id") =: i]) $ projectId proj)
    ++
    [ (u "name")          =: projectName proj
    , (u "owner")         =: projectOwner proj
    , (u "description")   =: projectDescription proj
    , (u "collaborators") =: projectCollaborators proj
    , (u "readers")       =: readersEitherToDoc (projectReaders proj) ]

  collectionName _ = "projects"

-- | Convert document to either.
readersDocToEither :: UserDefined -> Either Public [UserName]
readersDocToEither (UserDefined bs) = 
  case maybeRead . S8.unpack $ bs of
    Nothing -> error "readersDocToEither: failed to parse"
    Just e  -> e

-- | Convert either to document.
readersEitherToDoc :: Either Public [UserName] -> UserDefined
readersEitherToDoc = UserDefined . S8.pack . show


--
-- Misc
--

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
