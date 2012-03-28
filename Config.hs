{-# LANGUAGE Safe #-}
module Config ( gitstar_ssh_web_url
              , gitstar_ssh_web_authorization ) where

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Base64.URL as URL

gitstar_ssh_web_url :: String
gitstar_ssh_web_url = "http://localhost:9292/"

gitstar_ssh_web_authorization :: S8.ByteString
gitstar_ssh_web_authorization = S8.pack "Basic " `S8.append`
                                URL.encode (S8.pack "gitstar:w00t")
