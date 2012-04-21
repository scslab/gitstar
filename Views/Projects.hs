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

import Data.Monoid

import Models
import Data.List.Split
import Text.Blaze.Html5 hiding (title, style)
import Text.Blaze.Html5.Attributes hiding (label, form, span)

transformAppUrl :: String -> String -> String -> String
transformAppUrl url user proj =
  replace "$user" user $ replace "$project" proj url
  where replace wrd to = (join to) . (splitOn wrd)
        join joiner (x:[]) = x
        join joiner (x:xs) = x ++ joiner ++ (join joiner xs)
        join _ [] = []

showProject :: UserName -> Project -> [GitstarApp] -> Maybe Project -> Html
showProject user proj apps forkedProj = do
  let isCurUser = user == projectOwner proj
  div ! class_ "page-header" $
    h1 $ do
      toHtml $ projectName proj
      unless (isPublic proj) $
        i ! class_ "icon-lock" ! title "Private project" $ ""
      unless isCurUser $
        form ! action "/projects" ! method "POST" ! id "fork_proj" $ do
          input ! type_ "hidden" ! name "_fork"
                ! value (toValue . show . projectObjId $ proj)
          a ! href "#fork_proj"
            ! class_ "btn btn-primary gh-button fork icon white fork-proj" $ "Fork"
      when isCurUser $ small $ a ! href (toValue $ "/" ++ projectOwner proj ++ "/"
                              ++ projectName proj ++ "/edit") $ "edit"
  p ! class_ "well" $ toHtml $ let desc = projectDescription proj
                               in if null desc then "No description" else desc
  p $ do
    "Repo: "
    code $ do
      "git clone "
      strong $ toHtml $ user ++ "@gitstar.com:" ++ projectRepository proj
  case forkedProj of
    Nothing -> ""
    Just fp -> p $ do "Forked from: "
                      let url = "/" ++ projectOwner fp ++ "/" ++ projectName fp
                      a ! href (toValue url) $ toHtml url
  ul ! id "apps" ! class_ "nav nav-pills" $ do
    forM apps $ \app -> do
      li $ a ! class_ "external"
             ! href (toValue $ transformAppUrl (appUrl app) (projectOwner proj) (projectName proj))
             $ toHtml $ appName app
    when isCurUser $ li $ a ! href "#add_app" $ do "Add "; span ! class_ "icon-plus" $ ""
  iframe ! class_ "project_app" ! src "" $ ""
  div ! id "add_app" ! class_ "project_app" ! style "display: none" $ do
    h2 $ "Add an app to your project"
    div $ do
      input ! type_ "search" ! id "app_search" ! placeholder "Search for an app"
            ! dataAttribute "provide" "typeahead"
    form ! action (toValue $ "/" ++ projectOwner proj ++ "/" ++ projectName proj)
         ! method "POST" ! id "project" ! style "display: none" $ do
      div ! id "app_description" $ ""
      forM apps $ \app -> do
        input ! type_ "hidden" ! name "apps[]" ! value (toValue $ appId app)
      input ! type_ "hidden" ! name "apps[]" ! id "new_app"
      input ! type_ "submit" ! class_ "btn btn-primary" ! value "Add"

formProject :: Maybe Project -> Html
formProject mproj = do
  let act = toValue $ (maybe "/projects" (\proj -> "/" ++ projectOwner proj
                                              ++ "/" ++ projectName proj) mproj)
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
      label $ do "Collaborators (" 
                 a ! href "#" ! rel "tooltip"
                   ! title "Can push and pull repo" $ "?"
                 ")"
      input ! type_ "text" ! id "new_collaborator"
            ! placeholder "username"
      " "
      a ! href "#add_collaborator" $ do
        span ! class_ "icon-plus" $ ""
    ul ! id "collaborators" $ do
      input ! type_ "hidden" ! name "collaborators[]" ! value ""
      forM_ (filter (not . null) collaborators) $ \collaborator -> do
        li $ do
          input ! type_ "hidden" ! name "collaborators[]"
                ! value (toValue collaborator)
          a ! href (toValue $ "/" ++ collaborator) $ toHtml collaborator
          void "  "
          a ! href "#rm_collaborator"
            ! dataAttribute "name" (toValue collaborator) $ do
            span ! class_ "icon-minus" $ ""
    div $ do
      label $ do "Viewers ("
                 a ! href "#" ! rel "tooltip"
                   ! title "Can only pull from repo" $ "?"
                 ")"
      input ! type_ "text" ! id "new_reader"
            ! placeholder "username"
            ! (if projIsPub then disabled "" else mempty)
      " "
      a ! href "#add_reader" $ do
        span ! class_ "icon-plus" $ ""
    ul ! id "readers" $ do
      input ! type_ "hidden" ! name "readers[]" ! value ""
      forM_ (filter (not . null) readers) $ \reader -> do
        li $ do
          input ! type_ "hidden" ! name "readers[]"
                ! value (toValue reader)
          a ! href (toValue $ "/" ++ reader) $ toHtml reader
          void "  "
          a ! href "#rm_reader"
            ! dataAttribute "name" (toValue reader) $ do
            span ! class_ "icon-minus" $ ""
    div $
      button ! type_ "submit" $ "Submit"
  where projIsPub = maybe False isPublic mproj
        collaborators = maybe [] projectCollaborators mproj
        readers = case mproj of
          Nothing -> []
          Just proj -> either (const []) Prelude.id $ projectReaders proj

editProject :: Project -> Html
editProject proj = do
  div ! class_ "page-header" $
    h1 $ do
      toHtml $ projectName proj
      unless (isPublic proj) $
        i ! class_ "icon-lock" ! title "Private project" $ ""
  p $ a ! href (toValue $ "/" ++ projectOwner proj ++ "/" ++ projectName proj) $ "view"
  formProject $ Just proj

newProject :: Html
newProject = do
  div ! class_ "page-header" $
    h1 "New Project"
  formProject Nothing
