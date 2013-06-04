{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( (++)
  , withUserOrDoAuth
  , with404orJust
  , md5
  , redirectBack
  ) where

import Prelude hiding ((++))
import Data.Char (toLower)
import Data.Monoid
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import Hails.HttpServer
import Hails.Web.Controller
import Hails.Web.Responses
import Hails.Web.User
import qualified Crypto.Hash.MD5 as MD5
import Data.Hex

(++) :: Monoid a => a -> a -> a
(++) = mappend

with404orJust :: Maybe a -> (a -> Controller Response) -> Controller Response
with404orJust ma act = case ma of
                    Just a -> act a
                    Nothing -> return notFound

md5 :: T.Text -> String
md5 = (map toLower) . S8.unpack . hex . MD5.hash . S8.pack . T.unpack

