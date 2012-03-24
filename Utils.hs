{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isSpace)
import Data.Maybe (listToMaybe, fromJust)

import Data.IterIO.Http.Support
import Hails.Database.MongoDB (grantPriv)

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

doGetPolicyPriv policy = do
  app  <- (principal . fromJust) `liftM` requestHeader "x-hails-app"
  liftLIO $ do gate <- grantPriv policy app
               p <- getPrivileges
               callGate gate p

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

with404orJust mval act = case mval of
                           Nothing -> respond404
                           Just val -> act val

getHailsUser :: Monad m => Action t m String
getHailsUser = (S8.unpack . fromJust) `liftM` requestHeader "x-hails-user"
