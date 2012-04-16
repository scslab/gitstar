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
import Hails.Data.LBson (genObjectId)

import Control.Monad
import Control.Monad.Trans.State

import LIO
import LIO.DCLabel

import Hails.App
import Data.IterIO.Http (respAddHeader)


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


--
-- Flash notifications
--

-- | This sets the @_flash-*@ cookie value to the given message, with
-- a unique message ID.
flash :: String -> String -> Action t b DC ()
flash n msg = do
  oid <- liftLIO genObjectId
  modify $ \s ->
    let flashHeader = (S8.pack "Set-Cookie",
          S8.pack $ "_flash-" ++ n ++ "=" ++ show oid ++ "," ++ msg)
    in s { actionResp = respAddHeader flashHeader (actionResp s)}

flashInfo :: String -> Action t b DC ()
flashInfo = flash "info"

flashError :: String -> Action t b DC ()
flashError = flash "error"

flashSuccess :: String -> Action t b DC ()
flashSuccess = flash "success"
