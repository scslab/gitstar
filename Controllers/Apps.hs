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
import qualified Data.ByteString as S
import Data.Map (fromList, Map)
import qualified Data.Aeson as JSON (encode)
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
    atype <- requestHeader "accept"
    case atype >>= (S.findSubstring "application/json") of
      Just _ -> do
        apps <- liftLIO $ do
          policy <- gitstar
          eapps <- withDB policy $ do
            cur <- find $ select [] "apps"
            cursorToApps cur []
          either (fail . show) return $ eapps
        render "application/json" $ JSON.encode $ Prelude.map (\app ->
          fromList [ ("_id", appId app)
                   , ("title", appTitle app)
                   , ("name", appName app)
                   , ("description", appDescription app)
                   , ("url", appUrl app)
                   , ("owner", appOwner app)] :: Map String String) apps 
      _ -> do
        uName <- getHailsUser
        apps <- liftLIO $ do
          policy <- gitstar
          eapps <- withDB policy $ do
            cur <- find $ select [ "owner" =: uName ] "apps"
            cursorToApps cur []
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

-- | Unlabels each document in the result set of a cursor and
-- transforms it to a 'GitstarApp'.
cursorToApps :: Cursor DCLabel -> [GitstarApp] -> Action DCLabel TCBPriv () [GitstarApp]
cursorToApps cur arr = do
  nc <- next cur
  case nc of
    Just ldoc -> do
      doc <- liftLIO $ unlabel ldoc
      let resarr = maybe arr (:arr) (fromDocument doc)
      cursorToApps cur $ resarr
    Nothing -> return $ reverse arr

