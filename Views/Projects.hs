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

import Prelude hiding (div, id, span)
import qualified Prelude
import Control.Monad

import Models
import Data.List (intercalate)
import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (label, form, span)

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
  form ! action act ! method "POST" ! id "project" $ do
    case mproj of
      Just _ -> return () 
      Nothing -> div $ do
                   label "Name"
                   input ! type_ "text" ! name "name"
    div $ do
      label "Description"
      textarea ! name "description" $ toHtml $
        maybe "" projectDescription mproj
    div $
      label ! class_ "checkbox" $ do
        if projIsPub then
          input ! type_ "checkbox" ! name "public"
                ! checked "checked"
          else input ! type_ "checkbox" ! name "public"
        "Public?"
    div $ do
      label "Collaborators (can push and pull repo)"
      input ! type_ "text" ! id "new_collaborator"
            ! placeholder "username"
      " "
      a ! href "#add_collaborator" $ do
        span ! class_ "icon-plus" $ ""
    ul ! id "collaborators" $ do
      forM_ collaborators $ \collaborator -> do
        li $ do
          input ! type_ "hidden" ! name "collaborators[]"
              ! value (toValue collaborator)
          a ! href (toValue $ "/" ++ collaborator) $ toHtml collaborator
    div $ do
      label "Viewers (can only pull from repo)"
      input ! type_ "text" ! id "new_reader"
            ! placeholder "username"
      " "
      a ! href "#add_reader" $ do
        span ! class_ "icon-plus" $ ""
    ul ! id "readers" $ do
      forM_ readers $ \reader -> do
        li $ do
          input ! type_ "hidden" ! name "readers[]"
              ! value (toValue reader)
          a ! href (toValue $ "/" ++ reader) $ toHtml reader
    div $
      button ! type_ "submit" $ "Submit"
  where projIsPub = maybe False isPublic mproj
        collaborators = maybe [] projectCollaborators mproj
        readers = case mproj of
          Nothing -> []
          Just p -> either (const []) Prelude.id $ projectReaders p

editProject :: Project -> Html
editProject proj = do
  h1 $ do
    toHtml $ projectName proj
    unless (isPublic proj) $
      i ! class_ "icon-lock" ! title "Private project" $ ""
  p $ a ! href (toValue $ "/" ++ projectOwner proj ++ "/" ++ projectName proj) $ "view"
  formProject $ Just proj

newProject :: Html
newProject = do
  h1 "New Project"
  formProject Nothing
