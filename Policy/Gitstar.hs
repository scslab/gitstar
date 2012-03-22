{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, IncoherentInstances #-}
{-# LANGUAGE Safe #-}
module Policy.Gitstar ( gitstar
                      , GitstarPolicy
                      , UserName, User(..), SSHKey(..)
                      , ProjectId, Project(..), Public(..)
                      ) where

import Prelude hiding (lookup)

import Control.Monad (foldM)

import Data.Maybe (listToMaybe, fromJust)
import Data.Typeable
import Hails.Data.LBson hiding (map)

import Hails.Database
import Hails.Database.MongoDB hiding (map)
import Hails.Database.MongoDB.Structured

import LIO
import LIO.DCLabel

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
-- User model
--

-- | User name is simply  a stirng
type UserName = String

-- | An SSH key has a name and key value
data SSHKey = SSHKey { sshKeyId    :: ObjectId -- ^ ID
                     , sshKeyTitle :: !String  -- ^ Name
                     , sshKeyValue :: !Binary  -- ^ Actual key
                     } deriving (Show, Eq)

-- | Data type describing users
data User = User { userName     :: UserName    -- ^ User name
                 , userKeys     :: [SSHKey]    -- ^ User's ssh keys
                 , userProjects :: [ProjectId] -- ^ User's projects
                 } deriving (Show, Eq)

instance DCRecord User where
  fromDocument doc = do
    uName   <- lookup (u "_id") doc
    keyDocs <- lookup (u "keys") doc
    keys <- case mapM safeFromBsonDoc keyDocs of
               Nothing -> fail "fromDocument: safeFromBsonDoc failed"
               Just ks -> mapM docToSshKey ks
    uPrjs <- lookup (u "projects") doc
    return $ User { userName      = uName
                  , userKeys      = keys
                  , userProjects  = uPrjs }
      where docToSshKey :: Monad m => Document DCLabel -> m SSHKey
            docToSshKey doc = do
              i <- lookup (u "_id") doc
              t <- lookup (u "title") doc
              v <- lookup (u "value")  doc
              return $ SSHKey { sshKeyId    = i
                              , sshKeyTitle = t
                              , sshKeyValue = v }

  toDocument usr = [ (u "_id")      =: userName usr
                   , (u "keys")     =: (map sshKeyToDoc $ userKeys usr)
                   , (u "projects") =: userProjects usr ]
    where sshKeyToDoc :: SSHKey -> BsonDocument
          sshKeyToDoc k = fromJust $
            safeToBsonDoc ([ (u "_id")   =: sshKeyId k
                           , (u "title") =: sshKeyTitle k
                           , (u "value") =: sshKeyValue k ] :: Document DCLabel)

  collectionName _ = "users"





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
                      Right rs -> listToComponent [listToDisj $ rs ++ collabs]
            in newDC (r .\/. owner p)
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
