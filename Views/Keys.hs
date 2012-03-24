{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Keys where

import Prelude hiding (div, span)
import Control.Monad

import Models
import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (id, label, form, span)

import qualified Data.ByteString.Char8 as S8
import Hails.Data.LBson (Binary(..), ObjectId)

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
  div ! class_ "row" $ do
    div ! class_ "span8" $ h1 "SSH Keys"
    div ! class_ "span1 offset3" $ do
      a ! href "/keys/new" ! class_ "btn" $ do
        "New "
        span ! class_ "icon-plus" $ ""
  table ! class_ "table table-striped" $ do
    tr $ do
      th "Title"
      th "Fingerprint"
      th ""
    forM_ keys $ \k -> do
      tr $ do
        td $ toHtml (sshKeyTitle k)
        td $ toHtml $ showKeyVal (sshKeyValue k)
        --TODO: implement delete
        td $ a ! href (toValue $ "/keys/"++ show (sshKeyId k) ++ "/delete") $ do
             span ! class_ "icon-trash" $ ""
             "Remove"
newUserKey :: Html
newUserKey = do
  h1 "Register new key"
  formUserKey

-- | Show a Binary value
showKeyVal :: Binary -> String
showKeyVal (Binary bs) =
  let strVal = S8.unpack bs
  in take 30 strVal
