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
import Hails.Database.MongoDB (grantPriv, PrivilegeGrantGate)

import Control.Monad

import LIO
import LIO.DCLabel


-- | Force get parameter value
getParamVal n = (L8.unpack . paramValue . fromJust) `liftM` param n

-- | Get (maybe) paramater value and transform it with @f@
getMParamVal f n = fmap (f . paramValue) `liftM` param n

-- | Convert a Param comma separate value to a list of values
fromCSList :: Param -> [String]
fromCSList = map (strip . L8.unpack) . L8.split ',' . paramValue
  where strip = filter (not . isSpace)

-- | Get privileges based on current user
appGetPolicyPriv :: PrivilegeGrantGate dbp =>  dbp -> Action t DC DCPrivTCB
appGetPolicyPriv policy = do
  app <- (principal . fromJust) `liftM` requestHeader  "x-hails-app"
  doGetPolicyPriv policy app

-- | Get privileges based on current app
userGetPolicyPriv :: PrivilegeGrantGate dbp =>  dbp -> Action t DC DCPrivTCB
userGetPolicyPriv policy = do
  usr <- (principal . fromJust) `liftM` requestHeader "x-hails-user"
  doGetPolicyPriv policy usr

-- | Get privilege based on principal
doGetPolicyPriv :: PrivilegeGrantGate dbp => dbp -> Principal -> Action t DC DCPrivTCB
doGetPolicyPriv policy prin = liftLIO $ do
  gate <- grantPriv policy prin
  p <- getPrivileges
  callGate gate p

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

with404orJust mval act = case mval of
                           Nothing -> respond404
                           Just val -> act val

getHailsUser :: Monad m => Action t m String
getHailsUser = (S8.unpack . fromJust) `liftM` requestHeader "x-hails-user"
