{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Keys ( KeysController(..), listKeys ) where

import Control.Monad

import Models
import Layouts
import Utils
import Policy.Gitstar
import Views.Keys

import LIO
import LIO.DCLabel

import Data.Maybe (fromJust)
import Data.IterIO.Http.Support
import qualified Data.ByteString.Lazy as L

import Hails.App
import Hails.Data.LBson hiding (map, key)

data KeysController = KeysController

-- | List keys for the user specified by parameter @user_name@
listKeys :: Action t b DC  ()
listKeys = do
  uName <- getParamVal "user_name"
  curUser <- getHailsUser
  doListKeys (curUser == uName) uName

-- | Given a labeled username actually list the keys for the user.
doListKeys :: Bool -> UserName -> Action t b DC  ()
doListKeys updateFlag uName = do
  keys  <- liftLIO $ userKeys `liftM` getOrCreateUser uName
  atype <- requestHeader "accept"
  case atype of
    Just "application/bson" ->
      render "application/bson" $ encodeDoc $ mkDoc keys
    _ -> renderHtml $ keysIndex updateFlag keys
    where convert = fromJust . safeToBsonDoc . toDocument 
          mkDoc ks = fromJust . safeToBsonDoc $
                      (["keys" =: map convert ks] :: Document DCLabel)

instance RestController t (DCLabeled L.ByteString) DC KeysController where
  restIndex _ = getHailsUser >>= doListKeys True

  restNew _ = renderHtml newUserKey

  restCreate _ = do
    uName  <- getHailsUser
    ldoc   <- bodyToLDoc 
    void . liftLIO $ do luser  <- addUserKey uName ldoc
                        policy <- gitstar
                        saveLabeledRecord policy luser
    redirectTo "/keys"
