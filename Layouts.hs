{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE OverloadedStrings #-}
module Layouts where

import LIO
import Gitstar.Policy

import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8

import Control.Monad

import Hails.HttpServer
import Hails.Web.Controller hiding (body)
import Hails.Web.Responses

import Prelude hiding (head, id, div, span)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (title, span, content)
import qualified Text.Blaze.Html.Renderer.Utf8 as R (renderHtml)

import Utils

renderHtml :: Html -> Controller Response
renderHtml htmlBody = do
  muName <- getHailsUser
  muser <- liftLIO $ maybe (return Nothing) mkUser muName
  return $ okHtml $ R.renderHtml $ application muser htmlBody
    where mkUser uName = Just `liftM` getOrCreateUser uName

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue uri)

homeHtml :: Html -> Controller Response
homeHtml htmlBody = return $ okHtml $ R.renderHtml $ homeLayout htmlBody

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


application :: Maybe User -> Html -> Html
application muser content = docTypeHtml $ do
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
            maybe publicMenu userMenu muser
     div ! class_ "row" $
       div ! id "flash-messages" ! class_ "span4 offset4" $ ""
     div ! class_ "container" $ content
     script ! src "/static/js/jquery.js" $ ""
     script ! src "/static/js/jquery.cookie.js" $ ""
     script ! src "/static/js/bootstrap.min.js" $ ""
     script ! src "/static/js/bootstrap-typeahead.js" $ ""
     script ! src "/static/js/application.js" $ ""
     script ! src "/static/js/flash.js" $ ""
      where publicMenu = do
              li $ a ! href "/login" $ do
                span ! class_ "icon-road" $ ""
                " Login"
            userMenu user = do
              let gravatar = md5 . fromMaybe "" $ userGravatar user
              li $ a ! href (toValue $ "/" `mappend` userName user) $
                    img ! src (toValue $ "https://secure.gravatar.com/avatar/"
                                          `mappend` gravatar `mappend` "?s=25")
              li ! class_ "dropdown" $ do
              a ! href "#" ! class_ "dropdown-toggle" 
                ! dataAttribute "toggle" "dropdown" $ do
                  toHtml $ userName user
                  b ! class_ "caret" $ ""
              ul ! class_ "dropdown-menu" $ do
                li $ a ! href (toValue $ "/" `mappend` userName user) $ do
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

