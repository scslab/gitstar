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
  "hello"
