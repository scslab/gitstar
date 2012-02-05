{-# LANGUAGE OverloadedStrings #-}

module Controllers where

import Layouts

import Data.IterIO.Http.Support.RestController
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

data MessagesController = MessagesController

instance RestController MessagesController where
	restIndex _ = do
		renderHtml $ do
			h1 $ "Messages"
			p "Hello World"
