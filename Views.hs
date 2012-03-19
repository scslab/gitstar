{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE OverloadedStrings #-}

module Views where

import Prelude hiding (div)
import Control.Monad

import Models
import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (id, label, form)

showProject :: Project -> Html
showProject proj = do
  h1 $ do
    toHtml $ projectName proj
    unless (projectPublic proj) $
      i ! class_ "icon-lock" ! title "Private project" $ ""
  p $ a ! href (toValue $ "/projects/" ++ projectName proj ++ "/edit") $ "edit"
  p ! class_ "well" $ toHtml $ maybe "No description" id $ projectDescription proj
  p $ do "Repo: "; toHtml $ projectRepository proj
  h2 "Collaborators"
  ul $ do
    forM_ (projectCollaborators proj) (\col ->
      li $ toHtml col)

formProject :: Maybe Project -> Html
formProject mproj = do
  let act = toValue $ "/projects/" ++ (maybe "" projectName mproj)
  form ! action (act) ! method "POST" $ do
    case mproj of
      Just _ -> return ()
      Nothing -> do
        div $ do
          label "Name"
          input ! type_ "text" ! name "_id"
    div $ do
      label ! class_ "checkbox" $ do
        input ! type_ "checkbox" ! name "public"
              ! checked (toValue $ maybe ("" :: String) (\x -> if projectPublic x then "checked" else "") mproj)
        "Public?"
    div $ do
      label "Description"
      textarea ! name "description" $ toHtml $ maybe "" id $ do
        maybe Nothing projectDescription mproj
    div $ do
      label "Repository"
      input ! type_ "text" ! name "repository"
            ! value (toValue $ maybe "" projectRepository mproj)
    div $ do
      button ! type_ "submit" $ "Submit"

editProject :: Project -> Html
editProject proj = do
  h1 $ do
    toHtml $ projectName proj
    unless (projectPublic proj) $
      i ! class_ "icon-lock" ! title "Private project" $ ""
  p $ a ! href (toValue $ "/projects/" ++ projectName proj) $ "view"
  formProject $ Just proj

newProject :: Html
newProject = do
  h1 "New Project"
  formProject Nothing
