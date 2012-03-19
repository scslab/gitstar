{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Policy.Gitstar where

import Prelude hiding (lookup)

import Control.Monad (foldM)
import Data.Typeable
import Hails.Database
import Hails.Database.MongoDB
import LIO.DCLabel
import DCLabel.NanoEDSL

lcollections = newDC (<>) ("gitstar" :: String)
colClearance = newDC ("gitstar" :: String) (<>)

labelForProject doc = newDC collabsOrPublic collabs
  where collabs = maybe (<>)
          (listToComponent . (:[]) . listToDisj . (++ ["gitstar"]))
          (lookup "collaborators" doc :: Maybe [String])
        collabsOrPublic = case lookup "public" doc of
          Just True -> (<>)
          _ -> collabs

projectsCollection :: TCBPriv -> DC (Collection DCLabel)
projectsCollection p = collectionP p "projects" lpub colClearance $
  RawPolicy labelForProject
            [("name", SearchableField)]

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

  policyDB (GitstarPolicy _ db) = db

-- | Collection keeping track of user's ssh-keys.
usersCollection :: TCBPriv -> DC (Collection DCLabel)
usersCollection p = collectionP p "users" lpub colClearance $
  RawPolicy userLabel
            [ ("_id", SearchableField)
            , ("key",  SearchableField)
            ]
   where userLabel doc =
           let i = {-("_id" `at` doc :: String) .\/.  -}("gitstar" :: String)
           in  newDC (<>) i
