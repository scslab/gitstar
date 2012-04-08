{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isSpace)
import Data.Maybe (listToMaybe, fromJust)

import Data.IterIO.Http.Support
import Hails.Database.MongoDB ( Document
                              , grantPriv, PrivilegeGrantGate
                              , labeledDocI )

import Control.Monad

import LIO
import LIO.DCLabel

import Hails.App


-- | Force get parameter value
getParamVal :: Monad m => S8.ByteString -> Action t b m String
getParamVal n = (L8.unpack . paramValue . fromJust) `liftM` param n

-- | Get (maybe) paramater value and transform it with @f@
getMParamVal :: Monad m
             => (L8.ByteString -> a)
             -> S8.ByteString
             -> Action t b m (Maybe a)
getMParamVal f n = fmap (f . paramValue) `liftM` param n

-- | Convert a Param comma separate value to a list of values
fromCSList :: Param -> [String]
fromCSList = map (strip . L8.unpack) . L8.split ',' . paramValue
  where strip = filter (not . isSpace)

-- | Get privileges based on current user
appGetPolicyPriv :: PrivilegeGrantGate dbp => dbp -> Action t b DC DCPrivTCB
appGetPolicyPriv policy = do
  app <- principalS `liftM` getHailsApp
  doGetPolicyPriv policy app
    where principalS :: String -> Principal
          principalS = principal

-- | Get privilege based on principal
doGetPolicyPriv :: PrivilegeGrantGate dbp => dbp -> Principal -> Action t b DC DCPrivTCB
doGetPolicyPriv policy prin = liftLIO $ do
  gate <- grantPriv policy prin
  p <- getPrivileges
  callGate gate p

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

with404orJust :: Monad m => Maybe a -> (a -> Action t b m ()) -> Action t b m ()
with404orJust mval act = case mval of
                           Nothing -> respond404
                           Just val -> act val

-- | Convert the body to a labeled key-value Bson document.
bodyToLDoc :: Action t (DCLabeled L8.ByteString) DC (DCLabeled (Document DCLabel))
bodyToLDoc = do
 req  <- getHttpReq
 body <- getBody
 liftLIO $ labeledDocI req body
