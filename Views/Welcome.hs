{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Welcome where

import Prelude hiding (div, span)
import Control.Monad

import Models
import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (id, label, form, span)

import qualified Data.ByteString.Char8 as S8
import Hails.Data.LBson (Binary(..), ObjectId)

welcomeView :: Html
welcomeView = do
  div ! class_ "hero-unit" $ do
    h1 $ "Gitstar - Where loops count"
    p $ toHtml $ (concat 
          [ "Gitstar is a distruptive, extensible, HTML5 based,"
          , "community driven, social code sharing app full of Haskell"
          , "goodness. Look at your keyboard. Gitstar hosts your code"
          , "and presents an interface that's... just... magical! I'm on a"
          , "horse" ] :: String)
    p $ a ! href "/projects/new" ! class_ "btn btn-large btn-primary" $
          "Start coding!"
