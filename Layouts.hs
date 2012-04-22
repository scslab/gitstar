{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE OverloadedStrings #-}

module Layouts where

import LIO
import Policy.Gitstar
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IterIO.Http.Support

import Hails.App
import Hails.Crypto

import Prelude hiding (head, id, div, span)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (title, span, content)
import qualified Text.Blaze.Renderer.Utf8 as R (renderHtml)

renderHtml :: Html -> Action t b DC ()
renderHtml htmlBody = do
  uName <- getHailsUser
  user <- liftLIO $ getOrCreateUser uName
  render "text/html" $ R.renderHtml $ application user htmlBody

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue uri)

homeHtml :: Html -> Action t b DC ()
homeHtml htmlBody = render "text/html" $ R.renderHtml $ homeLayout htmlBody

homeLayout :: Html -> Html
homeLayout content = docTypeHtml $ do
  head $ do
    title $ "GitStar - For hackers and other heretics"
    stylesheet "/static/css/bootstrap.css"
    stylesheet "/static/css/gh-buttons.css"
    stylesheet "/static/css/home.css"
    body $ do
     div ! class_ "row" $
       div ! id "flash-messages" ! class_ "span4 offset4" $ ""
     div ! class_ "container" $ content
     script ! src "/static/js/jquery.js" $ ""
     script ! src "/static/js/jquery.cookie.js" $ ""
     script ! src "/static/js/bootstrap.min.js" $ ""
     script ! src "/static/js/bootstrap-typeahead.js" $ ""
     script ! src "/static/js/application.js" $ ""
     script ! src "/static/js/flash.js" $ ""


application :: User -> Html -> Html
application user content = docTypeHtml $ do
  head $ do
    title $ "GitStar - For hackers and other heretics"
    stylesheet "/static/css/bootstrap.css"
    stylesheet "/static/css/gh-buttons.css"
    stylesheet "/static/css/application.css"
    body $ do
     div ! class_ "navbar navbar-fixed-top" $ do
       div ! class_ "navbar-inner" $ do
         div ! class_ "container" $ do
           a ! href "/" ! class_ "brand" $ "Gitstar"
           ul ! class_ "nav" $ do
             li $ a ! href "/users" $ "List Users"
             li $ a ! href "/projects" $ "List Projects"
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
                li $ a ! href "/apps/" $ do
                  span ! class_ "icon-pencil" $ ""
                  " Manage Apps"
                li $ a ! href "/keys/" $ do
                  span ! class_ "icon-lock" $ ""
                  " Manage keys"
                li $ a ! href "/logout" $ do
                  span ! class_ "icon-road" $ ""
                  " Logout"
     div ! class_ "row" $
       div ! id "flash-messages" ! class_ "span4 offset4" $ ""
     div ! class_ "container" $ content
     script ! src "/static/js/jquery.js" $ ""
     script ! src "/static/js/jquery.cookie.js" $ ""
     script ! src "/static/js/bootstrap.min.js" $ ""
     script ! src "/static/js/bootstrap-typeahead.js" $ ""
     script ! src "/static/js/application.js" $ ""
     script ! src "/static/js/flash.js" $ ""

