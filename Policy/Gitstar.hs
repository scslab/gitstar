{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
module Policy.Gitstar ( gitstar
                      , GitstarPolicy
                      , UserName, User(..)
                      , ProjectId, Project(..), Public(..)
                      ) where

import Prelude hiding (lookup)

import Control.Monad (foldM)
import Data.Maybe (listToMaybe, fromJust)
import Data.Typeable

import Hails.Database
import Hails.Database.MongoDB
import Hails.Database.MongoDB.Structured

import LIO
import LIO.DCLabel
import DCLabel.Safe (priv)
import DCLabel.NanoEDSL

import qualified Data.ByteString.Char8 as S8

-- | Policy handler
gitstar :: DC GitstarPolicy
gitstar = mkPolicy

data GitstarPolicy = GitstarPolicy TCBPriv (Database DCLabel)
  deriving (Typeable)

instance DatabasePolicy GitstarPolicy where
  createDatabasePolicy conf priv = do
    db <- labelDatabase conf lcollections lpub
    db' <- foldM (\d col -> do
              c <- col priv
              assocCollectionP priv c d) db [ projectsCollection
                                            , usersCollection
                                            ]
    return $ GitstarPolicy priv db'
      where lcollections = newDC (<>) (owner priv)

  policyDB (GitstarPolicy _ db) = db

  policyOwner (GitstarPolicy p _) = principal . owner $ p

instance PrivilegeGrantGate GitstarPolicy where
  getGrantGate policy@(GitstarPolicy p _) app = getLabel >>= \curL ->
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

-- | Data type describing users
data User = User { userName     :: UserName    -- ^ User name
                 , userKey      :: Binary      -- ^ User's ssh key
                 , userProjects :: [ProjectId] -- ^ User's projects
                 } deriving (Show, Eq)

instance DCRecord User where
  fromDocument doc = do
    name <- lookup (u "_id") doc
    key  <- lookup (u "key") doc
    prjs <- lookup (u "projects") doc
    return $ User { userName      = name
                  , userKey       = key
                  , userProjects  = prjs }

  toDocument usr = [ (u "_id")      =: userName usr
                   , (u "key")      =: userKey usr
                   , (u "projects") =: userProjects usr ]

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
   where userLabel u = newDC (<>) ((userName u) .\/. (owner p))
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

-- | A data type describing a project
data Project = Project {
    projectId            :: ProjectId
    -- ^ Project id
  , projectName          :: String
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
      , projectReaders       = readersDoc2Either pRedrs }

  toDocument proj =
    (maybe [] (\i -> [(u "_id") =: i]) $ projectId proj)
    ++
    [ (u "name")          =: projectName proj
    , (u "owner")         =: projectOwner proj
    , (u "description")   =: projectDescription proj
    , (u "collaborators") =: projectCollaborators proj
    , (u "readers")       =: readersEither2Doc (projectReaders proj) ]

  collectionName _ = "projects"

-- | Convert document to either.
readersDoc2Either :: UserDefined -> Either Public [UserName]
readersDoc2Either (UserDefined bs) = 
  case maybeRead . S8.unpack $ bs of
    Nothing -> error "readersDoc2Either: failed to parse"
    Just e  -> e

-- | Convert either to document.
readersEither2Doc :: Either Public [UserName] -> UserDefined
readersEither2Doc = UserDefined . S8.pack . show


--
-- Misc
--

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
