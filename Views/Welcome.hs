{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Welcome ( welcomeView, goodbyeView ) where

import Prelude hiding (div, span)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (id, label, form, span)

welcomeView :: Html
welcomeView = do
  div ! class_ "hero-unit" $ do
    h1 $ "GitStar"
    p $ do toHtml $ (concat 
            [ "GitStar is a social source code management platform "
            , "built using the new Hails web framework. "
            , "Unlike GitHub and the like, GitStar is composed of many "
            , "different applications, safely operating on your data. "
            ] :: String)
           "Take a look at the "
           a ! href "/scs/hails" $ "/scs/hails"
           " project: the code viewer and wiki are \"third-party\" apps!"
    p $ do toHtml (concat
             [ " What does this mean? Well, did you "
             , "ever want a feature in GitHub or a different way to implement "
             , "a functionality (e.g., different news feed)? With GitStar you "
             , "can build these features as apps and integrate them into "
             , "your projects immediately... No longer will you have to wait"
             , "month, weeks, or days for features!"
             ]  :: String)
    p $ do a ! href "/scs/hails/page/Tutorial.md"
             ! class_ "btn btn-large btn-success" $
             "Get started!"
           "   "
           a ! href "/projects/new" ! class_ "btn btn-large btn-primary" $
             "Start coding!"
           "   "
           a ! href "/scs/hails/page/Architecture.md"
             ! class_ "btn btn-large" $
             "About Hails?"

goodbyeView :: String -> Html
goodbyeView user = do
  div ! class_ "hero-unit" $ do
    h1 $ toHtml $ "Goodbye "++ user
    p $ "Hope to see you again soon..."
