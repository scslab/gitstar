{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers where

import Prelude hiding (id)

import Layouts

import LIO (liftLIO, getLabel, getClearance)
import LIO.DCLabel (DC, DCLabel)
import Hails.Database
import Hails.Database.MongoDB hiding (unpack)

import Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Char8 as S
import Data.IterIO.Http.Support.Action
import Data.IterIO.Http.Support.RestController
import Text.Blaze.Html5 hiding (param)
import Text.Blaze.Html5.Attributes hiding (form, label)

data UsersController = UsersController

instance RestController DC UsersController where
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
    (Just username) <- requestHeader "authorization"
    email <-  fromParam "email"
    name <- fromParam "name"
    website <- fromParam "website"
    (Right uid) <- liftLIO $ withDB "gitstar" $ do
      insert "users" ([ "name" := (val $ unpack name)
                      , "email" := (val $ unpack email)
                      , "website" := (val $ unpack website)
                      , "username" := (val $ S.unpack username)
                      ] :: Document DCLabel)
    renderHtml $ do
      h1 $ do "Welcome to Gitstar "; toHtml $ unpack name; "!"
      p $ do "UID: "; toHtml $ show uid
             table $ do tr $ do
                          td "E-mail: "
                          td $ toHtml.unpack $ email
                        tr $ do
                          td "Website/Blog: "
                          td $ toHtml.unpack $ website
    where fromParam str = param str >>= \p -> return $ maybe "" (paramValue) p

data MessagesController = MessagesController

instance Monad m => RestController m MessagesController where
  restIndex _ = do
    renderHtml $ do
      h1 $ "Messages"
      p "Hello World"
