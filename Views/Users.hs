{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Users ( showUser
--                   , editUserKey
                   , newUserKey
                   ) where

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
        li $ a ! href (toValue $ "/projects/" ++ show (projectObjId proj)) $
                      toHtml (projectName proj)
  h2 "SSH Keys"
  forM_ (userKeys user) $ \k -> do
    h3 $ toHtml (sshKeyTitle k)
    p $ a ! href (toValue $ "/users/" ++ userName user ++ "/keys/"
                                      ++ show (sshKeyId k)) $ "edit"
    p ! class_ "well" $ toHtml $ showKeyVal (sshKeyValue k)

formUserKey :: UserName -> ObjectId -> Maybe SSHKey -> Html
formUserKey uName kid mkey = 
  form ! action act ! method "POST" $ do
    div $ do
      label "User name"
      input ! type_ "text" ! disabled "true" ! value (toValue uName)
    div $ do
      label "Key ID"
      input ! type_ "text" ! disabled "true" ! value (toValue (show kid))
    div $ do
      label "Key title"
      input ! type_ "text" ! name "ssh_key_title"
            ! value (toValue keyName)
    div $ do
      label "Key"
      textarea ! name "ssh_key_value" $ toHtml keyVal
    div $ button ! type_ "submit" $ "Add key"
      where act = toValue $ "/users/" ++ uName
            (keyName, keyVal) = case mkey of
              Just k -> (sshKeyTitle k, showKeyVal . sshKeyValue $ k) 
              _ -> ("","")

{-
editUserKey :: UserName -> SSHKey -> Html
editUserKey uName k = do
  h1 $ toHtml uName
  p $ a ! href (toValue $ "/users/" ++ uName) $ "view"
  formUserKey uName (Just k)
  -}

newUserKey :: UserName -> ObjectId -> Html
newUserKey uName kid = do
  h1 "Register new key"
  formUserKey uName kid Nothing

-- | Show a Binary value
showKeyVal :: Binary -> String
showKeyVal (Binary bs) = S8.unpack bs 
