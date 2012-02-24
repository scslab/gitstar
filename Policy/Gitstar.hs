{-# LANGUAGE OverloadedStrings #-}
module Policy.Gitstar where

import Hails.Database.MongoDB
import LIO.DCLabel
import DCLabel.NanoEDSL

lcollections = newDC (<>) ("gitstar" :: String)
lpub = newDC (<>) (<>)

usersCollection :: DC (Collection DCLabel)
usersCollection = collection "users" lpub lpub $
  RawPolicy (\_ -> lpub) [("username", SearchableField)]

configDB :: DBConf -> DC (Database DCLabel)
configDB conf = do
  db <- labelDatabase conf lcollections lpub
  myUsersCollection <- usersCollection
  let priv = dbConfPriv conf
  assocCollectionP priv myUsersCollection db

