{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module Controllers.Projects ( ProjectsController(..) ) where

import Prelude hiding (lookup)

import Layouts
import Models
import Policy.Gitstar
import Utils
import Views.Projects

import LIO
import LIO.DCLabel

import Hails.Database.MongoDB (select, (=:))

import Data.Maybe (fromJust, fromMaybe)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IterIO.Http
import Data.IterIO.Http.Support

import Hails.Data.LBson (cast', ObjectId, encodeDoc)

import Control.Monad (liftM)

data ProjectsController = ProjectsController

contentType :: Monad m => Action t m S8.ByteString
contentType = do
  mctype <- requestHeader "accept"
  return $ fromMaybe "text/plain" mctype

instance RestController DC ProjectsController where
  restShow _ projectName = do
    policy <- liftLIO gitstar
    (Just uName) <- param "user_name"
    projM <- liftLIO $ findWhere policy $ select [ "name" =: L8.unpack projectName
                                                 , "owner" =: (L8.unpack $ paramValue uName)]
                                                 "projects"
    case projM of
      Just proj -> do
        ctype <- contentType
        case ctype of
          "application/bson" -> render "application/bson" $ encodeDoc $ toDocument proj
          _ -> renderHtml $ showProject proj
      Nothing   -> respond404

  restEdit _ projectName = do
    policy <- liftLIO gitstar
    (Just uName) <- param "user_name"
    projM <- liftLIO $ findWhere policy $ select [ "name" =: L8.unpack projectName
                                                 , "owner" =: (L8.unpack $ paramValue uName)]
                                                 "projects"
    case projM of
      Just proj -> renderHtml $ editProject proj
      Nothing -> respond404

  restNew _ = renderHtml $ newProject

  restCreate _ = do
    policy <- liftLIO gitstar
    pOwner <- (S8.unpack . fromJust) `liftM` requestHeader "x-hails-user"
    pName  <- getParamVal "name"
    pPub   <- maybe False (const True) `liftM` param "public"
    pRedrs <- maybe [] fromCSList `liftM` param "readers"
    pDesc  <- getParamVal "description"
    pColls <- maybe [] fromCSList `liftM` param "collaborators"
    let proj = Project { projectId            = Nothing
                       , projectName          = pName 
                       , projectOwner         = pOwner
                       , projectDescription   = pDesc 
                       , projectCollaborators = pColls
                       , projectReaders       = if pPub then Left Public
                                                        else Right pRedrs
                       } 
    privs <- doGetPolicyPriv policy
    erf <- liftLIO $ insertRecordP privs policy proj
    case erf of
      Right r -> redirectTo $ "/" ++ pOwner ++ "/" ++ pName
      _      -> respondStat stat500

  restUpdate _ projName = do
    policy <- liftLIO gitstar
    uName <- getParamVal "user_name"
    projM <- liftLIO $ findWhere policy $ select [ "name" =: L8.unpack projName
                                                 , "owner" =: uName]
                                                 "projects"
    case projM of
      Just proj -> do
        pPub   <- maybe False (const True) `liftM` param "public"
        pRedrs <- maybe [] fromCSList `liftM` param "readers"
        pDesc  <- getParamVal "description"
        pColls <- maybe [] fromCSList `liftM` param "collaborators"
        let projFinal = proj { projectDescription   = pDesc 
                             , projectCollaborators = pColls
                             , projectReaders       = if pPub then Left Public
                                                              else Right pRedrs
                             }
        privs <- doGetPolicyPriv policy
        erf <- liftLIO $ saveRecordP privs policy projFinal
        case erf of
          Right _ -> redirectTo $ "/" ++ projectOwner projFinal ++ "/" ++ (projectName projFinal)
          _      -> respondStat stat500
      Nothing -> respond404

