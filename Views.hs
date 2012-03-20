{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE OverloadedStrings #-}

module Views where

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
  p $ a ! href (toValue $ "/projects/" ++ (show . projectObjId $ proj) ++ "/edit") $ "edit"
  p ! class_ "well" $ toHtml $ let desc = projectDescription proj
                               in if null desc then "No description" else desc
  p $ do "Repo: "; toHtml $ projectRepository proj
  h2 "Collaborators"
  ul $ do
    forM_ (projectCollaborators proj) (\col ->
      li $ toHtml col)

formProject :: Maybe Project -> Html
formProject mproj = do
  let act = toValue $ "/projects/" ++ (maybe "" (show . projectObjId) mproj)
  form ! action act ! method "POST" $ do
    case mproj of
      Just _ -> return ()
      Nothing -> do
        div $ do
          label "Name"
          input ! type_ "text" ! name "name"
    div $ do
      label ! class_ "checkbox" $ do
        input ! type_ "checkbox" ! name "public"
              ! checked (toValue $ if projIsPub
                                    then "checked" else "" :: String )
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
                Just p -> either (const "") (intercalate ",") $ projectReaders p

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
