{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Users  where

import Prelude hiding (div, span, (++))
import Control.Monad

import Gitstar.Models
import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (id, label, form, span)

import Data.Maybe
import Utils

listUsers :: [User] -> Html
listUsers users = do
  div ! class_ "page-header" $
    h1 $ "GitStar Users"
  ul $ do
    forM_ users $ \user -> do
      li $ p $ a ! href (toValue $ "/" ++ userName user) $ do
        let gravatar = show . md5 $ fromMaybe "" $ userGravatar user
        img ! src (toValue $ "https://secure.gravatar.com/avatar/" ++ gravatar ++ "?s=25")
        " "
        toHtml $ userName user
        maybe (return ()) (\x -> toHtml $ "(" ++ x ++ ")") $ userFullName user

showUser :: User -> [Project] -> Html
showUser user projs = do
  div ! class_ "page-header" $
    h1 $ do
      let gravatar = show.md5 $ fromMaybe "" $ userGravatar user
      img ! src (toValue $ "https://secure.gravatar.com/avatar/" ++ gravatar ++ "?s=48")
      toHtml $ " " ++ userName user
  p ! class_ "well" $ do
    case userFullName user of
      Just fn -> do
        toHtml $ "Name: " ++ fn
        br
      _ -> ""
    case userCity user of
      Just city -> do
        toHtml $ "Location: " ++ city
        br
      _ -> ""
    case userWebsite user of
      Just website -> do
        void "Website: "
        a ! href (toValue website) $ toHtml website
        br
      _ -> ""
  unless (null projs) $ do
    h2 $ toHtml $ "Projects (" ++ (show . length $ projs) ++ ")"
    ul $ forM_ projs $ \proj ->
        li $ a ! href (toValue $ "/" ++ (userName user) ++ "/" ++ (projectName proj)) $
                      toHtml (projectName proj)

editUser :: User -> Html
editUser user = do
  div ! class_ "page-header" $
    h1 $ toHtml $ "My profile (" ++ userName user ++ ")"
  formUser $ Just user

formUser :: Maybe User -> Html
formUser muser = 
  form ! action "/user" ! method "POST" $ do
    case muser of
      Just _ -> input ! type_ "hidden"
                      ! name "_method"
                      ! value "PUT"
      _ -> return ()
    div $ do
      label "Name"
      input ! type_ "text" ! name "full_name"
            ! (value $ toValue $ att userFullName)
    div $ do
      label "City"
      input ! type_ "text" ! name "city"
            ! (value $ toValue $ att userCity)
    div $ do
      label "Website"
      input ! type_ "url" ! name "website"
            ! (value $ toValue $ att userWebsite)
    div $ do
      label "Gravatar E-mail"
      input ! type_ "email" ! name "gravatar"
            ! (value $ toValue $ att userGravatar)
    div $ button ! type_ "submit" $ "Submit"
  where att fn = case muser of
                    Just user -> maybe "" id $ fn user
                    Nothing -> ""

