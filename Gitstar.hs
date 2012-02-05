module Gitstar where

import qualified Data.ByteString.Char8 as S8
import Hails.HttpServer
import Data.IterIO.Http
import Data.IterIO.Server.TCPServer
import LIO.DCLabel
import LIO
import System.Environment

server :: DCPrivTCB -> HttpRequestHandler DC ()
server priv = do
  runHttpRoute $ routeFileSys systemMimeMap (dirRedir "index.html") "static"

