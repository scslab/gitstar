{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module Layouts where

import Prelude hiding (head, id, div)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (title)
import qualified Text.Blaze.Renderer.Utf8 as R (renderHtml)
import Data.IterIO.Http.Support.Responses

renderHtml body = render "text/html" $ R.renderHtml $ application body

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue uri)

application :: Html -> Html
application content = docTypeHtml $ do
  head $ do
    title $ "GitStar - Where loops count"
    stylesheet "/css/bootstrap.css"
    stylesheet "/css/application.css"
    body $ do
     div ! class_ "navbar navbar-fixed-top" $ do
       div ! class_ "navbar-inner" $ do
         div ! class_ "container" $ do
           h1 $ a ! href "/" ! class_ "brand" $ "Gitstar"
           ul ! class_ "nav pull-right" $ do
            li $ a ! href "/projects/new" $ "New project"
     div ! class_ "container" $ do
       content
     script ! src "js/bootstrap.min.js" $ ""
