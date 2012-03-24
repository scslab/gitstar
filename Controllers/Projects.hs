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

import Data.Maybe (fromJust, fromMaybe, isJust)
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
  -- /projects/:id where :id is the project id
  -- or
  -- /:user_name/:id where :id is the project name
  restShow _ projIdOrName = do
    mProj  <- getProj
    with404orJust mProj $ \proj -> do
      atype <- requestHeader "accept"
      case atype of
        Just "application/bson" ->
          render "application/bson" $ encodeDoc $ toDocument proj
        _ -> renderHtml $ showProject proj
    where getProj = do
            policy <- liftLIO gitstar
            muName <- param "user_name"
            case muName of
              Just uName ->
                let qry = select [ "name"  =: L8.unpack projIdOrName
                                 , "owner" =: (L8.unpack $ paramValue uName)]
                                 "projects"
                in liftLIO $ findWhere policy qry
              Nothing ->
                let mOid = maybeRead (L8.unpack projIdOrName) :: Maybe ObjectId
                in maybe (return Nothing)
                         (liftLIO . findBy policy "projects" "_id") mOid

  -- /projects/:id where :id is the project id
  restEdit _ pid = do
    let mOid = maybeRead (L8.unpack pid) :: Maybe ObjectId
    with404orJust mOid $ \oid -> do
      policy <- liftLIO gitstar
      mProj  <- liftLIO $ findBy policy "projects" "_id" oid
      with404orJust mProj $ \proj -> renderHtml $ editProject proj

  -- /projects/new
  restNew _ = renderHtml newProject

  restCreate _ = do
    policy <- liftLIO gitstar
    pOwner <- getHailsUser
    pName  <- getParamVal "name"
    pPub   <- isJust `liftM` param "public"
    pRedrs <- maybe [] fromCSList `liftM` param "readers"
    pDesc  <- getParamVal "description"
    pColls <- maybe [] fromCSList `liftM` param "collaborators"
    let proj = Project { projectId            = Nothing
                       , projectName          = pName 
                       , projectOwner         = pOwner
                       , projectDescription   = pDesc 
                       , projectCollaborators = pColls
                       , projectReaders       = if pPub then Left Public
                                                        else Right pRedrs } 
    exists <- projExists policy pOwner pName
    if exists
      then redirectTo "/projects/new" --TODO: print error "Project with this name exists"
      else do privs <- doGetPolicyPriv policy
              erf <- liftLIO $ insertRecordP privs policy proj
              case erf of
                Right r -> case cast' r of
                  Just oid -> do
                     oldU <- liftLIO $ getOrCreateUser pOwner
                     let usr = oldU { userProjects = Just oid : userProjects oldU }
                     liftLIO $ saveRecordP privs policy usr
                     redirectTo $ "/projects/" ++ show (oid :: ObjectId)
                  Nothing  ->
                     redirectTo $ "/users/" ++ projectOwner proj
                                            ++ "/" ++ projectName proj
                _      -> respondStat stat500
      where projExists policy owner projName = do
              let qry = select ["name" =: projName, "owner" =: owner] "projects"
              mproj <- liftLIO $ findWhere policy qry
              return $ case mproj of 
                         (Just (Project {})) -> True
                         _                   -> False

  restUpdate _ pid = do
    policy <- liftLIO gitstar
    let oid = read (L8.unpack pid) :: ObjectId
    projM <- liftLIO $ findBy policy "projects" "_id" oid
    case projM of
      Just proj -> do
        pPub   <- isJust `liftM` param "public"
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
          Right _ -> redirectTo $ "/projects/" ++ (show . projectObjId $ projFinal)
          _       -> respondStat stat500
      Nothing -> respond404
