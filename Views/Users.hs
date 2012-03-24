{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Users ( showUser
                   , newUserKey
                   , keysIndex
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
        li $ a ! href (toValue $ "/" ++ (userName user) ++ "/" ++ (projectName proj)) $
                      toHtml (projectName proj)

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
