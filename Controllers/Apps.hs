{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Apps ( appsController ) where

import LIO

import qualified Data.ByteString.Char8 as S8
import Data.Map (fromList, Map)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Aeson as JSON (encode)
import Gitstar.Policy
import Views.Apps
import Layouts
import Hails.HttpServer
import Hails.Database hiding (lookup)
import Hails.Database.Structured
import Hails.Web.REST
import Hails.Web.Responses
import Hails.Web.Controller

import Utils

appsController :: RESTController ()
appsController = do
  index $ withUserOrDoAuth $ \uName -> do
    req <- request >>= liftLIO . unlabel
    let atype = fromMaybe "" $ lookup "accept" (requestHeaders req)
    case S8.breakSubstring "application/json" atype of
      (_,y) | S8.null y -> do
        apps <- liftLIO $ withGitstar $
            findAll $ select [ "owner" -: uName ] "apps"
        renderHtml $ appsIndex apps
            | otherwise -> do
        apps <- liftLIO $ withGitstar $
            findAll $ select [ "owner" -: uName ] "apps"
        respond $ ok "application/json" $ JSON.encode $ Prelude.map (\app ->
          fromList [ ("_id", appId app)
                   , ("title", appTitle app)
                   , ("name", appName app)
                   , ("description", appDescription app)
                   , ("url", appUrl app)
                   , ("owner", appOwner app)] :: Map T.Text T.Text) apps 

  edit $ withUserOrDoAuth $ \user -> do
    (Just aid) <- queryParam "id"
    mapp <- liftLIO $ withGitstar $
      findBy "apps" "_id" $ S8.unpack aid
    case mapp of
      Just app -> renderHtml $ editApp app user
      Nothing -> return notFound

  new $ withUserOrDoAuth $ \user ->
    renderHtml $ newApp user
{-
  restCreate _ = withUserOrDoAuth $ \_ -> do
    policy <- liftLIO gitstar
    ldoc   <- bodyToLDoc
    liftLIO $ withDB policy $ do
      insert "apps" ldoc
    redirectTo "/apps"

  restUpdate _ aid = withUserOrDoAuth $ \_ -> do
    policy <- liftLIO gitstar
    ldoc   <- bodyToLDoc
    doc <- liftLIO $ unlabel ldoc
    if at "_id" doc == (L8.unpack aid) then do
      liftLIO $ withDB policy $ do
        save "apps" ldoc
      redirectTo "/apps"
      else respondStat stat403
-}

