{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module Layouts where


import LIO
import Policy.Gitstar
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IterIO.Http.Support

import Prelude hiding (head, id, div)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (title)
import qualified Text.Blaze.Renderer.Utf8 as R (renderHtml)
import Data.IterIO.Http.Support.Responses

renderHtml body = do
  policy <- liftLIO gitstar
  uName <- (S8.unpack . fromJust) `liftM` requestHeader "x-hails-user"
  user <- liftLIO $ getOrCreateUser uName
  render "text/html" $ R.renderHtml $ application user body

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue uri)

application :: User -> Html -> Html
application user content = docTypeHtml $ do
  head $ do
    title $ "GitStar - Where loops count"
    stylesheet "/static/css/bootstrap.css"
    stylesheet "/static/css/application.css"
    body $ do
     div ! class_ "navbar navbar-fixed-top" $ do
       div ! class_ "navbar-inner" $ do
         div ! class_ "container" $ do
           h1 $ a ! href "/" ! class_ "brand" $ "Gitstar"
           ul ! class_ "nav pull-right" $ do
            li $ a ! href "/user" $ table $ tr $ do
              let gravatar = fromMaybe "" $ userGravatar user
              td $ img ! src (toValue $ "https://secure.gravatar.com/avatar/" ++ gravatar ++ "?s=140")
                  ! height "20" ! width "20"
              td $ toHtml $ userName user
            li $ a ! href "/projects/new" $ "New project"
            li $ a ! href "/keys/new" $ "New key"
     div ! class_ "container" $ do
       content
     script ! src "/static/js/bootstrap.min.js" $ ""

