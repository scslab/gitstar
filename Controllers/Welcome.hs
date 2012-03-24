{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Welcome ( welcome ) where

import Layouts
import Views.Welcome

import LIO
import LIO.DCLabel

import Data.IterIO.Http
import Data.IterIO.Http.Support

welcome :: Action t DC ()
welcome = do
  renderHtml welcomeView
