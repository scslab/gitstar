{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}

module Models where

import Prelude hiding (lookup)

import LIO
import LIO.DCLabel
import Data.String
import Hails.Database
import Hails.Database.MongoDB
import Hails.Data.LBson

import Data.Monoid (mappend)

class DCRecord a where
  -- | Convert a document to a record
  fromDocument :: Document DCLabel -> DC a
  -- | Convert a record to a document
  toDocument :: a -> Document DCLabel
  -- | Get the collection name for the record
  collectionName :: a -> String
  -- | Find an object with mathing value for the given key
  findBy :: (Val DCLabel v, DatabasePolicy p)
         => p -> CollectionName -> Key -> v -> DC (Maybe a)
  -- | Insert a record into the database
  insertRecord :: (DatabasePolicy p)
               => p -> CollectionName -> a -> DC (Either Failure ())
  -- | Insert a record into the database
  saveRecord :: (DatabasePolicy p)
             => p -> CollectionName -> a -> DC (Either Failure ())
  -- | Same as 'findBy', but using explicit privileges.
  findByP :: (Val DCLabel v, DatabasePolicy p)
          => DCPrivTCB -> p -> CollectionName -> Key -> v -> DC (Maybe a)
  -- | Same as 'insertRecord', but using explicit privileges.
  insertRecordP :: (DatabasePolicy p)
              => DCPrivTCB -> p -> CollectionName -> a -> DC (Either Failure ())
  -- | Same as 'saveRecord', but using explicit privileges.
  saveRecordP :: (DatabasePolicy p)
              => DCPrivTCB -> p -> CollectionName -> a -> DC (Either Failure ())

  --
  -- Default definitions
  --

  --
  findByP priv policy colName key val = do
    result <- withPrivileges noPrivs $ 
                 withDB policy $ findOne $ select [key =: val] colName
    case result of
      Right (Just p) -> unlabelP priv p >>= fromDocument >>= (return . Just )
      _ -> return Nothing
  --
  findBy = findByP noPrivs
  --
  insertRecordP p policy colName record = do
    priv <- getPrivileges
    withDB policy $ do
      insertP_ (priv `mappend` p)  colName $ toDocument record
  --
  insertRecord = insertRecordP noPrivs
  --
  saveRecordP p policy colName record = do
    priv <- getPrivileges
    withDB policy $ do
      saveP (priv `mappend` p) colName $ toDocument record
  --
  saveRecord = saveRecordP noPrivs
  --

data Project = Project {
    projectName :: String
  , projectDescription :: Maybe String
  , projectRepository :: String
  , projectCollaborators :: [String]
  , projectPublic :: Bool
  } deriving (Show)

instance DCRecord Project where
  fromDocument doc = do
    pName <- lookup (pack "_id") doc
    let pDesc = lookup (pack "description") doc
    pRepo <- lookup (pack "repository") doc
    pCollabs <- lookup (pack "collaborators") doc
    let pPublic = maybe False id $ lookup (pack "public") doc
    return $ Project { projectName = pName
                     , projectDescription = pDesc
                     , projectRepository = pRepo
                     , projectCollaborators = pCollabs
                     , projectPublic = pPublic}

  toDocument proj = [ (pack "_id") =: projectName proj
                    , (pack "description") =: projectDescription proj
                    , (pack "collaborators") =: projectCollaborators proj
                    , (pack "repository") =: projectRepository proj
                    , (pack "public") =: projectPublic proj
                    ]

  collectionName _ = "projects"

--
-- Users
--

data User = User { userName :: String -- ^ User name
                 , userKey  :: Binary -- ^ User's ssh key
                 } deriving (Show, Eq)

instance DCRecord User where
  fromDocument doc = do
    name <- lookup (u "_id") doc
    key  <- lookup (u "key") doc
    return $ User { userName = name
                  , userKey  = key }

  toDocument proj = [ (u "_id") =: userName proj
                    , (u "key") =: userKey proj ]

  collectionName _ = "users"
