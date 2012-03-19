{-# LANGUAGE FlexibleContexts #-}

module Models where

import Prelude hiding (lookup)

import LIO
import LIO.DCLabel
import Data.String
import Hails.Database
import Hails.Database.MongoDB
import Hails.Data.LBson

class DCRecord a where
  fromDocument :: Document DCLabel -> DC a
  toDocument :: a -> Document DCLabel
  collectionName :: a -> String
  findBy :: (Val DCLabel v, DatabasePolicy p)
         => p -> CollectionName -> Key -> v -> DC (Maybe a)
  findBy policy colName key val = withPrivileges noPrivs $ do
    result <- withDB policy $ findOneP noPrivs $ select [key =: val] colName
    case result of
      Right (Just p) -> unlabel p >>= fromDocument >>= (return . Just )
      _ -> return Nothing
  insertRecord :: (DatabasePolicy p) => p -> CollectionName -> a -> DC (Either Failure ())
  insertRecord policy colName record = do
    priv <- getPrivileges
    withDB policy $ do
      insertP_ priv colName $ toDocument record
  saveRecord :: (DatabasePolicy p) => p -> CollectionName -> a -> DC (Either Failure ())
  saveRecord policy colName record = do
    priv <- getPrivileges
    withDB policy $ do
      saveP priv colName $ toDocument record

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

