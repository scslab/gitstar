{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Welcome ( welcomeView, goodbyeView ) where

import Prelude hiding (id, div, span)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (label, form, span)

welcomeView :: String -> Html
welcomeView username = do
  div ! class_ "hero-unit" $ do
    div ! class_ "page-header" $
      h1 $ "GitStar"
    p $ do toHtml $ (concat 
            [ "GitStar is a social source code management platform "
            , "built using the new Hails web framework. "
            , "Gitstar provides your traditional web-based code hosting "
            , "site with a twist: "
            , "Insead of a single codebase, GitStar is composed of many "
            , "applications, written by different people, "
            , "safely operating on your data. "
            ] :: String)
           "Take a look at the "
           a ! href "Hails" $ "/scs/hails"
           " project: the code viewer and wiki are \"third-party\" apps!"
    p $ br
    div ! class_ "row-fluid" $ do
      div ! class_ "span3" $ a ! href (toValue $ "/" ++ username) ! class_ "btn btn-large btn-primary" $
          "View My Profile"
      div ! class_ "span3" $ a ! href "/keys" ! class_ "btn btn-large btn-primary" $
          "Manage SSH Keys"
      div ! class_ "span3" $ a ! href "/projects/new" ! class_ "btn btn-large btn-primary" $
          "Create a Project"
      div ! class_ "span3" $ a ! href "/apps/new" ! class_ "btn btn-large btn-primary" $
          "Register an App"

goodbyeView :: String -> Html
goodbyeView user = do
  div ! class_ "hero-unit" ! id "logout" $ do
    div ! class_ "page-header" $
      h1 $ toHtml $ "Goodbye "++ user
    p $ "Hope to see you again soon..."
