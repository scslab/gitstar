{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Apps ( AppsController(..) ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IterIO.Http (stat403)
import Data.IterIO.Http.Support hiding (Action)
import Policy.Gitstar
import Views.Apps
import Layouts
import Utils
import Hails.App
import Hails.Database
import Hails.Database.MongoDB.Structured
import Hails.Database.MongoDB hiding (reverse)

data AppsController = AppsController

instance RestController t (DCLabeled L8.ByteString) DC AppsController where
  restIndex _ = do
    uName <- getHailsUser
    apps <- liftLIO $ do
      policy <- gitstar
      eapps <- withDB policy $ do
        cur <- find $ select [ "owner" =: uName ] "apps"
        convertToApps cur []
      either (fail . show) return $ eapps
    renderHtml $ appsIndex apps

  restEdit _ aid = do
    user <- getHailsUser
    mapp <- liftLIO $ do
      policy <- gitstar
      findBy policy "apps" "_id" $ L8.unpack aid
    with404orJust mapp $ \app -> do
      renderHtml $ editApp app user

  restNew _ = do
    user <- getHailsUser
    renderHtml $ newApp user

  restCreate _ = do
    policy <- liftLIO gitstar
    ldoc   <- bodyToLDoc
    liftLIO $ withDB policy $ do
      insert "apps" ldoc
    redirectTo "/apps"

  restUpdate _ aid = do
    policy <- liftLIO gitstar
    ldoc   <- bodyToLDoc
    doc <- liftLIO $ unlabel ldoc
    if at "_id" doc == (L8.unpack aid) then do
      liftLIO $ withDB policy $ do
        save "apps" ldoc
      redirectTo "/apps"
      else respondStat stat403

convertToApps :: Cursor DCLabel -> [GitstarApp] -> Action DCLabel TCBPriv () [GitstarApp]
convertToApps cur arr = do
  nc <- next cur
  case nc of
    Just ldoc -> do
      doc <- liftLIO $ unlabel ldoc
      let resarr = maybe arr (:arr) (fromDocument doc)
      convertToApps cur $ resarr
    Nothing -> return $ reverse arr

