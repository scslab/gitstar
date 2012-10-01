{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( (++)
  , withUserOrDoAuth
  , with404orJust
  , md5
  , getHailsUser
  , requestHeader
  , redirectBack
  ) where

import Prelude hiding ((++))
import Hails.Data.Hson (Document)
import Data.Bson.Binary
import Data.Binary.Put
import Data.Monoid
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import Gitstar.Models
import Hails.HttpServer
import Hails.Web.Controller
import Hails.Web.Responses
import LIO
import Network.HTTP.Types.Header

(++) :: Monoid a => a -> a -> a
(++) = mappend

withUserOrDoAuth :: (UserName -> Controller Response) -> Controller Response
withUserOrDoAuth act = getHailsUser >>= \muser ->
  case muser of
    Just user -> act user
    Nothing -> return $ Response status200 [("X-Hails-Login", "Yes")] ""

with404orJust :: Maybe a -> (a -> Controller Response) -> Controller Response
with404orJust ma act = case ma of
                    Just a -> act a
                    Nothing -> return notFound

md5 :: a -> a
md5 g = g

getHailsUser :: Controller (Maybe UserName)
getHailsUser = do
  fmap (fmap (T.pack . S8.unpack)) $ requestHeader "x-hails-user"

requestHeader :: HeaderName -> Controller (Maybe S8.ByteString)
requestHeader name = do
  req <- request >>= liftLIO . unlabel
  return $ lookup name $ requestHeaders req

redirectBack :: Controller Response
redirectBack = do
  mrefr <- requestHeader "referer"
  return $ case mrefr of
    Just refr -> redirectTo $ S8.unpack refr
    Nothing -> redirectTo "/"

