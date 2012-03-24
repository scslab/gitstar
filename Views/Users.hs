{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Users  where

import Prelude hiding (div)
import Control.Monad

import Models
import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (id, label, form)

import qualified Data.ByteString.Char8 as S8
import Hails.Data.LBson (Binary(..), ObjectId)

showUser :: User -> [Project] -> Html
showUser user projs = do
  h1 $ toHtml $ userName user
  unless (null projs) $ do
    h2 "Projects"
    ul $ forM_ projs $ \proj ->
        li $ a ! href (toValue $ "/" ++ (userName user) ++ "/" ++ (projectName proj)) $
                      toHtml (projectName proj)

editUser :: User -> Html
editUser user = do
  h1 $ toHtml $ "My profile (" ++ userName user ++ ")"
  formUser $ Just user

formUser :: Maybe User -> Html
formUser muser = 
  form ! action "/user" ! method "POST" $ do
    div $ do
      label "Name"
      input ! type_ "text" ! name "full_name"
            ! (value $ toValue $ att userFullName)
    div $ do
      label "City"
      input ! type_ "text" ! name "city"
            ! (value $ toValue $ att userCity)
    div $ do
      label "Gravatar E-mail"
      input ! type_ "email" ! name "gravatar"
            ! (value $ toValue $ att userGravatar)
    div $ button ! type_ "submit" $ "Submit"
  where att fn = case muser of
                    Just user -> maybe "" id $ fn user
                    Nothing -> ""

formUserKey :: Html
formUserKey = 
  form ! action "/keys" ! method "POST" $ do
    div $ do
      label "Key title"
      input ! type_ "text" ! name "ssh_key_title"
    div $ do
      label "Key"
      textarea ! name "ssh_key_value" $ ""
    div $ button ! type_ "submit" $ "Add key"


keysIndex :: [SSHKey] -> Html
keysIndex keys = do
  h1 "SSH Keys"
  table ! class_ "table table-striped table-bordered" $ do
    tr $ do
      th "Title"
      th "Fingerprint"
    forM_ keys $ \k -> do
      tr $ do
        td $ toHtml (sshKeyTitle k)
        td $ toHtml $ showKeyVal (sshKeyValue k)

newUserKey :: Html
newUserKey = do
  h1 "Register new key"
  formUserKey

-- | Show a Binary value
showKeyVal :: Binary -> String
showKeyVal (Binary bs) =
  let strVal = S8.unpack bs
  in take 30 strVal
