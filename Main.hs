import qualified Data.ByteString.Char8 as S8
import Data.IterIO.HttpRoute
import Data.IterIO.Server.TCPServer
import System.Environment


main :: IO ()
main = do
  port <- fmap read $ getEnv "PORT"
  runTCPServer $ simpleHttpServer (fromInteger port) $ runHttpRoute $ routeFileSys mimeMap (dirRedir "index.html") "static/"

mimeMap :: String -> S8.ByteString
mimeMap "html" = S8.pack "text/html"
mimeMap "css" = S8.pack "text/css"
