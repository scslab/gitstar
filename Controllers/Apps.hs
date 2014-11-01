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
import qualified Data.ByteString.Lazy.Char8 as L8
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
import Debug.Trace

appsController :: RESTController
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

  create $ withUserOrDoAuth $ \user -> do
    lreq <- request
    liftLIO $ do
      ldoc  <- labeledRequestToHson lreq
      withGitstar $ insert_ "apps" ldoc
    respond $ redirectTo "/apps"

  update $ withUserOrDoAuth $ \_ -> do
    (Just aid) <- queryParam "id"
    lreq <- request
    ldoc  <- liftLIO $ labeledRequestToHson lreq
    doc   <- liftLIO $ unlabel ldoc
    if at "_id" doc == (S8.unpack aid) 
      then do withGitstar $ save "apps" ldoc
              respond $ redirectTo "/apps"
      else respond forbidden
