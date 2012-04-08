{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, IncoherentInstances #-}
module Policy.Gitstar ( gitstar
                      , GitstarPolicy
                      -- * Privileged insert/delete
                      , gitstarInsertRecord
                      , gitstarInsertLabeledRecord
                      , gitstarSaveRecord
                      , gitstarSaveLabeledRecord
                      -- * Projects
                      , ProjectId, Project(..), Public(..)
                      , mkProject
                      , updateUserWithProjId
                      , partialProjectUpdate
                      -- * Users
                      , UserName, User(..), SSHKey(..)
                      , getOrCreateUser
                      , partialUserUpdate 
                      , addUserKey 
                      ) where

import Prelude hiding (lookup)

import Control.Monad

import Data.Char (isSpace)
import Data.Maybe
import Data.Typeable
import Hails.Data.LBson hiding ( map, head, break
                               , tail, words, key, filter
                               , dropWhile, split, foldl
                               , notElem)

import Hails.App
import Hails.Database
import Hails.Database.MongoDB hiding ( Action, map, head, break
                                     , tail, words, key, filter
                                     , dropWhile, split, foldl
                                     , notElem)
import Hails.Database.MongoDB.Structured

import qualified Data.ByteString.Char8 as S8

import LIO.MonadCatch

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
                      

instance MkToLabeledDocument GitstarPolicy where
  mkToLabeledDocument (GitstarPolicy privs _) = toDocumentP privs
    

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
-- Insert and save recors with gitstar privileges
--

-- | Insert a record into the gitstar database, using privileges to
-- downgrade the current label for the insert.
gitstarInsertRecord :: DCRecord a => a -> DC (Either Failure (Value DCLabel))
gitstarInsertRecord = gitstarInsertOrSaveRecord insertRecord

-- | Insert a labeled record into the gitstar database, using privileges to
-- downgrade the current label for the insert.
gitstarInsertLabeledRecord :: DCLabeledRecord a
                           => DCLabeled a -> DC (Either Failure (Value DCLabel))
gitstarInsertLabeledRecord =
  gitstarInsertOrSaveLabeledRecord insertLabeledRecord

-- | Save a record into the gitstar database, using privileges to
-- downgrade the current label for the save.
gitstarSaveRecord :: DCRecord a => a -> DC (Either Failure ())
gitstarSaveRecord = gitstarInsertOrSaveRecord saveRecord

-- | Save a labeled record into the gitstar database, using privileges to
-- downgrade the current label for the save.
gitstarSaveLabeledRecord :: DCLabeledRecord a
                           => DCLabeled a -> DC (Either Failure ())
gitstarSaveLabeledRecord = gitstarInsertOrSaveLabeledRecord saveLabeledRecord

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
            either (err "Failed to create user") (const $ return newUser) res
    where newUser = User { userName = uName
                         , userKeys = []
                         , userProjects = []
                         , userFullName = Nothing
                         , userCity = Nothing
                         , userWebsite = Nothing
                         , userGravatar = Nothing }
          err m _ = throwIO . userError $ m

-- | Execute a \"toLabeled\" gate with gitstar prvileges
-- Note this downgrades the current label of the inner computation and
-- so privileges and so should not be exported.
gitstarToLabeled :: DCLabeled (Document DCLabel)
                 -> (Document DCLabel -> DC a) -> DC (DCLabeled a)
gitstarToLabeled ldoc act = do
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
  gitstarToLabeled ldoc $ \partialDoc ->
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
  gitstarToLabeled ldoc $ \doc -> do
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
                                    projectOwner proj:(rs ++ collabs)]
            in newDC (owner p .\/. r)
                     (projectOwner proj .\/.  owner p)

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

instance DCLabeledRecord Project where

-- | Convert document to either.
readersDocToEither :: UserDefined -> Either Public [UserName]
readersDocToEither (UserDefined bs) = 
  case maybeRead . S8.unpack $ bs of
    Nothing -> error "readersDocToEither: failed to parse"
    Just e  -> e

-- | Convert either to document.
readersEitherToDoc :: Either Public [UserName] -> UserDefined
readersEitherToDoc = UserDefined . S8.pack . show

-- | Given a username and a labeled document corresponding to a key,
-- find the user in the DB and return a 'User' value with the key
-- added. The resultant value is endorsed by the policy/service.
mkProject :: UserName -> DCLabeled (Document DCLabel)
              -> DC (DCLabeled Project)
mkProject username ldoc = do
  void $ getOrCreateUser username
  gitstarToLabeled ldoc $ \doc ->
    let doc' = projectFormatFields doc
    -- create project object:
    in fromDocument $ merge [ "owner"  =: username ] doc'

-- | Convert HTTP input parameters to correct (DCRecord) format
projectFormatFields :: Document DCLabel -> Document DCLabel
projectFormatFields doc0 =
  let doc1 = if hasField "collaborators"
               then fixCollabs doc0
               else doc0
      doc2 = if hasField "public"
               then merge [ "readers" =:
                             (readersEitherToDoc $ Left Public)] doc1
               else if hasField "readers"
                      then fixReaders doc1
                      else doc1
  in doc2
  where hasField f = foldl (\a (k := _) -> (k == u f) || a) False doc0
        fixCollabs d =
          let cs = fromCSList . fromJust $ lookup (u "collaborators") d
          in merge ["collaborators" =: cs] d
        fixReaders d =
          let rs = fromCSList . fromJust $ lookup (u "readers") d
          in merge ["readers" =: (readersEitherToDoc $ Right rs)] d
        --
        fromCSList rs = map strip $ split ',' rs
        strip = filter (not . isSpace)

-- | Given a user name and project ID, associate the project with the
-- user, if it's not already.
updateUserWithProjId :: UserName -> ProjectId -> DC ()
updateUserWithProjId username oid = do
  policy@(GitstarPolicy privs _) <- gitstar
  muser <- findBy policy  "users"    "_id" username
  mproj <- findBy policy  "projects" "_id" oid
  case (muser, mproj) of
    (Just usr, Just proj) -> do
      unless (username == projectOwner proj) $ err "User is not project owner"
      let projIds = userProjects usr
          newUser = usr { userProjects = oid : projIds }
      when (oid `notElem` projIds) $ void $ saveRecordP privs policy newUser
    _ -> err  "Expected valid user and project"
  where err = throwIO . userError

-- | Given a user name, project name, and partial project document,
-- retrive the project and merge the provided fields.
partialProjectUpdate :: UserName
                     -> ProjectName
                     -> DCLabeled (Document DCLabel)
                     -> DC (DCLabeled Project)
partialProjectUpdate username projname ldoc = do
  policy <- gitstar
  mproj <- findWhere policy $ select [ "name" =: projname
                                     , "owner" =: username ]
                                     "projects"
  case mproj of
    Just (proj@Project{}) -> gitstarToLabeled ldoc $ \doc ->
             -- Do not touch the user name and projects:
         let doc0 = projectFormatFields $ exclude ["_id"] doc
             doc1 = toDocument proj
         in fromDocument $ merge doc0 doc1 -- create new user
    _ -> err  "Expected valid user and project"
  where err = throwIO . userError


--
-- Misc
--

-- | Like 'read', but does not fail hard.
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Split a string into list of strings given a separator.
split   :: Char -> String -> [String]
split _ [] = []
split c s =  case dropWhile (==c) s of
                "" -> []
                s' -> w : split c s''
                      where (w, s'') = break (==c) s'

-- | Insert or save a labeled record using gitstar privileges to
-- untaint the current label (for the duration of the insert or save).
gitstarInsertOrSaveRecord :: DCRecord a
  => (GitstarPolicy -> a -> DC (Either Failure b))
  -> a
  -> DC (Either Failure b)
gitstarInsertOrSaveRecord f rec = do
  policy@(GitstarPolicy privs _) <- gitstar
  l <- getLabel
  withLabel privs (newDC (<>) (integrity l)) $ f policy rec

-- | Insert or save a labeled record using gitstar privileges to
-- untaint the current label (for the duration of the insert or save)
-- and record.
gitstarInsertOrSaveLabeledRecord :: DCLabeledRecord a
  => (GitstarPolicy -> DCLabeled a -> DC (Either Failure b))
  -> DCLabeled a
  ->  DC (Either Failure b)
gitstarInsertOrSaveLabeledRecord f lrec = do
  policy@(GitstarPolicy privs _) <- gitstar
  lcur <- getLabel
  lrec' <- untaintLabeledP privs (newDC (<>) (integrity . labelOf $ lrec)) lrec
  withLabel privs (newDC (<>) (integrity lcur)) $ f policy lrec'
