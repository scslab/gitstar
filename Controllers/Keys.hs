{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Keys ( KeysController(..)) where

import Control.Monad
import Control.Monad.Trans

import Models
import Layouts
import Utils
import Policy.Gitstar
import Views.Keys

import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.Http.Support
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Hails.App
import Hails.Data.LBson hiding (map)
import Hails.Database.MongoDB (labeledDocI)

data KeysController = KeysController (DCLabeled L8.ByteString)

contentType :: Monad m => Action t m S8.ByteString
contentType = do
  mctype <- requestHeader "accept"
  return $ fromMaybe "text/plain" mctype

doListKeys :: UserName -> Action t DC  ()
doListKeys uName = do
  keys <- liftLIO $ fmap userKeys $ getOrCreateUser uName
  atype <- requestHeader "accept"
  case atype of
    Just "application/bson" ->
      render "application/bson" $ encodeDoc $ mkDoc keys
    _ -> renderHtml $ keysIndex keys
    where convert = fromJust . safeToBsonDoc . toDocument 
          mkDoc ks = fromJust . safeToBsonDoc $
                      (["keys" =: map convert ks] :: Document DCLabel)

instance RestController a DC KeysController where
  restIndex _ = do
    uName <- getHailsUser
    doListKeys uName

  restNew _ = renderHtml newUserKey

  restCreate (KeysController lbody) = do
    uName <- getHailsUser
    req <- getHttpReq
    ldoc <- liftLIO $ labeledDocI req lbody
    liftLIO $ addKeyToUser uName ldoc
    redirectTo "/keys"
      where strictify = S.concat . L.toChunks

