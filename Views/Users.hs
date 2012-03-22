{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Users ( showUser
                   , editUser
                   , newUser
                   ) where

import Prelude hiding (div)
import Control.Monad

import Models
import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (id, label, form)

import qualified Data.ByteString.Char8 as S8
import Hails.Data.LBson (Binary(..))

showUser :: User -> [Project] -> Html
showUser user projs = do
  h1 $ toHtml $ userName user
  p $ a ! href (toValue $ "/users/" ++ userName user ++ "/edit") $ "edit"
  h2 "SSH Key"
  p ! class_ "well" $ toHtml $ showUserKey user
  h2 "Projects"
  ul $ forM_ projs $ \proj ->
      li $ a ! href (toValue $ "/projects/" ++ show (projectObjId proj)) $
                    toHtml (projectName proj)

formUser :: Maybe User -> Html
formUser muser = do
  let act = toValue $ "/users/" ++ maybe "" userName muser
  form ! action act ! method "POST" $ do
    div $ do
      label "SSH Key"
      textarea ! name "ssh_key" $ toHtml $ maybe "" showUserKey muser
    div $ button ! type_ "submit" $ "Submit"

editUser :: User -> Html
editUser user = do
  h1 $ toHtml $ userName user
  p $ a ! href (toValue $ "/users/" ++ userName user) $ "view"
  formUser $ Just user

newUser :: UserName -> Html
newUser n = do
  h1 "Register"
  h3 $ toHtml $ "User: " ++ n
  formUser Nothing

showUserKey :: User -> String
showUserKey user = let (Binary bs) = userKey user
                   in S8.unpack bs
