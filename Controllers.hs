{-# LANGUAGE OverloadedStrings #-}

module Controllers where

import Prelude hiding (id)

import Layouts

import Data.ByteString.Lazy.Char8
import Data.IterIO.Http.Support.Action
import Data.IterIO.Http.Support.RestController
import Text.Blaze.Html5 hiding (param)
import Text.Blaze.Html5.Attributes hiding (form, label)

data UsersController = UsersController

instance RestController UsersController where
	restNew _ = do
		renderHtml $ do
			h1 $ "Register an account"
			form ! action "/users" ! method "POST" $ do
				label $ do
					"Email:"
					input ! type_ "email" ! name "email" ! placeholder "princess@solofam.me"
				label $ do
					"Display Name:"
					input ! type_ "text" ! name "name" ! placeholder "Leia Organa"
				label $ do
					"Website/Blog:"
					input ! type_ "text" ! name "website" ! placeholder "http://ob1fan.wordpress.com"
				input ! type_ "submit" ! class_ "btn"

	restCreate _ = do
		email <-  fromParam "email"
		name <- fromParam "name"
		website <- fromParam "website"
		renderHtml $ do
			h1 $ do "Welcome to Gitstar "; toHtml $ unpack name; "!"
			table $ do
				tr $ do
					td "E-mail: "
					td $ toHtml.unpack $ email
				tr $ do
					td "Website/Blog: "
					td $ toHtml.unpack $ website
		where fromParam str = param str >>= \p -> return $ maybe "" (paramValue) p

data MessagesController = MessagesController

instance RestController MessagesController where
	restIndex _ = do
		renderHtml $ do
			h1 $ "Messages"
			p "Hello World"
