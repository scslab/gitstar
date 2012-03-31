{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Projects ( showProject
                      , editProject
                      , newProject
                      ) where

import Prelude hiding (div)
import Control.Monad

import Models
import Data.List (intercalate)
import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (id, label, form)

showProject :: Project -> Html
showProject proj = do
  h1 $ do
    toHtml $ projectName proj
    unless (isPublic proj) $
      i ! class_ "icon-lock" ! title "Private project" $ ""
  p $ a ! href (toValue $ "/" ++ projectOwner proj ++ "/" ++ projectName proj ++ "/edit") $ "edit"
  p ! class_ "well" $ toHtml $ let desc = projectDescription proj
                               in if null desc then "No description" else desc
  p $ toHtml $ "Repo: " ++ projectRepository proj
  h2 "Collaborators"
  ul $ forM_ (projectCollaborators proj) (li . toHtml)

formProject :: Maybe Project -> Html
formProject mproj = do
  let act = toValue $ (maybe "/projects" (\proj -> "/" ++ projectOwner proj ++ "/" ++ projectName proj) mproj)
  form ! action act ! method "POST" $ do
    case mproj of
      Just _ -> return ()
      Nothing -> do
        div $ do
          label "Name"
          input ! type_ "text" ! name "name"
    div $ do
      label ! class_ "checkbox" $ do
        if projIsPub then
          input ! type_ "checkbox" ! name "public"
                ! checked "checked"
          else input ! type_ "checkbox" ! name "public"
        "Public?"
    div $ do
      label "Readers"
      input ! type_ "text" ! name "readers"
            ! value (toValue $ readers)
    div $ do
      label "Description"
      textarea ! name "description" $ toHtml $ maybe "" id $ do
        maybe Nothing (Just . projectDescription) mproj
    div $ do
      label "Collaborators"
      input ! type_ "text" ! name "collaborators"
            ! value (toValue $ collaborators)
    div $ do
      button ! type_ "submit" $ "Submit"
        where projIsPub = maybe False isPublic mproj
              collaborators =
                maybe "" (intercalate "," .  projectCollaborators) mproj
              readers = case mproj of
                Nothing -> ""
                Just r -> either (const "") (intercalate ",") $ projectReaders r

editProject :: Project -> Html
editProject proj = do
  h1 $ do
    toHtml $ projectName proj
    unless (isPublic proj) $
      i ! class_ "icon-lock" ! title "Private project" $ ""
  p $ a ! href (toValue $ "/projects/" ++ (show . projectObjId $ proj)) $ "view"
  formProject $ Just proj

newProject :: Html
newProject = do
  h1 "New Project"
  formProject Nothing
