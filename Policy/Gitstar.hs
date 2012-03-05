{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Policy.Gitstar where

import Data.Typeable
import Hails.Database.MongoDB
import LIO.DCLabel
import DCLabel.NanoEDSL

lcollections = newDC (<>) ("gitstar" :: String)

usersCollection :: DC (Collection DCLabel)
usersCollection = collection "users" lpub lpub $
  RawPolicy (\_ -> lpub) [("username", SearchableField)]

data GitstarPolicy = GitstarPolicy TCBPriv (Database DCLabel)
  deriving (Typeable)

instance DatabasePolicy GitstarPolicy where
  createDatabasePolicy conf = do
    db <- labelDatabase conf lcollections lpub
    myUsersCollection <- usersCollection
    let priv = dbConfPriv conf
    res <- assocCollectionP priv myUsersCollection db
    return $ GitstarPolicy priv res

  policyDB (GitstarPolicy _ db) = db

