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

import Hails.Crypto

import Prelude hiding (head, id, div, span)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (title, span)
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
           a ! href "/" ! class_ "brand" $ "Gitstar"
           ul ! class_ "nav pull-right" $ do
            let gravatar = show.md5 $ L8.pack $ fromMaybe "" $ userGravatar user
            li $ a ! href (toValue $ "/" ++ userName user) $
                  img ! src (toValue $ "https://secure.gravatar.com/avatar/" ++ gravatar ++ "?s=25")
            li ! class_ "dropdown" $ do
              a ! href "#" ! class_ "dropdown-toggle" ! dataAttribute "toggle" "dropdown"
                $ do
                  toHtml $ userName user
                  b ! class_ "caret" $ ""
              ul ! class_ "dropdown-menu" $ do
                li $ a ! href (toValue $ "/" ++ userName user) $ do
                  span ! class_ "icon-user" $ ""
                  " View Profile"
                li $ a ! href "/user/edit" $ do
                  span ! class_ "icon-edit" $ ""
                  " Edit Profile"
                li $ a ! href "/projects/new" $ do
                  span ! class_ "icon-folder-open" $ ""
                  " New project"
                li $ a ! href "/keys/" $ do
                  span ! class_ "icon-lock" $ ""
                  " Manage keys"
     div ! class_ "container" $ do
       content
     script ! src "/static/js/jquery.js" $ ""
     script ! src "/static/js/bootstrap.min.js" $ ""

