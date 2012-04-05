{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Keys where

import Prelude hiding (div, span)
import Control.Monad

import Models
import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (id, label, form, span)


formUserKey :: Html
formUserKey = 
  form ! action "/keys" ! method "POST" $ do
    div $ do
      label "Key title"
      input ! type_ "text" ! name "title"
    div $ do
      label "Key"
      textarea ! name "value" $ ""
    div $ button ! type_ "submit" $ "Add key"


keysIndex :: Bool -> [SSHKey] -> Html
keysIndex updateFlag keys = do
  div ! class_ "row" $ do
    div ! class_ "span8" $ h1 "SSH Keys"
    when updateFlag $ div ! class_ "span1 offset3" $ do
      a ! href "/keys/new" ! class_ "btn" $ do
        void "New "
        span ! class_ "icon-plus" $ ""
  table ! class_ "table table-striped" $ do
    tr $ do
      th "Title"
      th "Fingerprint"
      when updateFlag $ th ""
    forM_ keys $ \k -> do
      tr $ do
        td $ toHtml (sshKeyTitle k)
        td $ toHtml $ fingerprint k
        --TODO: implement delete
        when updateFlag $ td $ a ! href (toValue $ "/keys/" ++
                                          show (sshKeyId k) ++ "/delete") $ do
             span ! class_ "icon-trash" $ ""
             "Remove"

newUserKey :: Html
newUserKey = do
  h1 "Register new key"
  formUserKey

