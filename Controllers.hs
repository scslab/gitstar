{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers where

import Prelude hiding (lookup)

import Layouts
import Models
import Policy.Gitstar
import Views

import LIO
import LIO.DCLabel

import qualified Data.ByteString.Char8 as S
import Data.ByteString.Lazy.Char8
import Data.IterIO.Http
import Data.IterIO.Http.Support

data ProjectsController = ProjectsController

instance RestController DC ProjectsController where
  restShow _ pid = do
    policy <- liftLIO gitstar
    projM <- liftLIO $ findBy policy "projects" "_id" $ unpack pid
    case projM of
      Just proj -> renderHtml $ showProject proj
      Nothing -> respond404

  restEdit _ pid = do
    policy <- liftLIO gitstar
    projM <- liftLIO $ findBy policy "projects" "_id" $ unpack pid
    case projM of
      Just proj -> renderHtml $ editProject proj
      Nothing -> respond404

  restNew _ = renderHtml $ newProject

  restCreate _ = do
    policy <- liftLIO gitstar
    (Just user) <- requestHeader "x-hails-user"
    pName <- fmap (maybe undefined (unpack . paramValue)) $ param "_id"
    pRepo <- fmap (maybe undefined (unpack . paramValue)) $ param "repository"
    pPublic <- fmap (maybe False (const True)) $ param "public"
    pDesc <- param "description"
    let proj = Project {
        projectName = pName
      , projectDescription = fmap (unpack . paramValue) $ pDesc
      , projectRepository = pRepo
      , projectCollaborators = [S.unpack user]
      , projectPublic = pPublic
    }
    erf <- liftLIO $ insertRecord policy "projects" proj
    case erf of
      Right _ -> redirectTo $ "/projects/" ++ (projectName proj)
      Left f -> respondStat stat500

  restUpdate _ pid = do
    policy <- liftLIO gitstar
    projM <- liftLIO $ findBy policy "projects" "_id" $ unpack pid
    case projM of
      Just proj -> do
        pRepo <- fmap (maybe undefined (unpack . paramValue)) $ param "repository"
        pPublic <- fmap (maybe False (const True)) $ param "public"
        pDesc <- param "description"
        let projFinal = proj {
            projectDescription = fmap (unpack . paramValue) $ pDesc
          , projectRepository = pRepo
          , projectPublic = pPublic
        }
        erf <- liftLIO $ saveRecord policy "projects" projFinal
        case erf of
          Right _ -> redirectTo $ "/projects/" ++ (projectName projFinal)
          Left f -> respondStat stat500
      Nothing -> respond404

