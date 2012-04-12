{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Apps where

import Prelude hiding (div, span)
import Control.Monad

import Models
import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (id, label, form, span)


appForm :: Maybe GitstarApp -> UserName -> Html
appForm mapp user = 
  form ! action (toValue $ "/apps/" ++ maybe "" appId mapp) ! method "POST" $ do
    input ! type_ "hidden" ! value (toValue user) ! name "owner"
    case mapp of
      Just app -> input ! type_ "hidden" ! name "_id" ! value (toValue $ appId app)
      Nothing -> div $ do
        label "Unique Id"
        input ! type_ "text" ! name "_id" ! placeholder "myappid"
    div $ do
      label "Name"
      input ! type_ "text" ! name "name"
            ! value (toValue $ maybe "" appName mapp)
    div $ do
      label "Title"
      input ! type_ "text" ! name "title"
            ! value (toValue $ maybe "" appTitle mapp)
    div $ do
      label "URL"
      input ! type_ "url" ! name "url"
            ! value (toValue $ maybe "" appUrl mapp)
      "?repo=user/repo"
    div $ do
      label "Description"
      textarea ! name "description" $ toHtml $
        maybe "" appDescription mapp
    div $ button ! type_ "submit" $ "Submit"

appsIndex :: [GitstarApp] -> Html
appsIndex apps = do
  h1 "My Gitstar Apps"
  p $ a ! class_ "btn btn-primary" ! href "/apps/new" $ do
    span ! class_ "icon-plus icon-white" $ ""
    " New App"
  ul ! class_ "thumbnails" $ forM_ apps $ \app -> do
    li ! class_ "span3" $ do
      div ! class_ "thumbnail" $ do
        h2 $ toHtml $ appName app
        p $ a ! href (toValue $ "/apps/" ++ appId app ++ "/edit") $ "edit"
        p $ do
          toHtml $ "Title: " ++ appTitle app
          br
          toHtml $ appUrl app
        p ! class_ "well" $ toHtml $ appDescription app

newApp :: UserName -> Html
newApp user = do
  h2 "New App"
  appForm Nothing user

editApp :: GitstarApp -> UserName -> Html
editApp app user = do
  h2 "Edit App"
  appForm (Just app) user

