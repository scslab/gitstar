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

import Hails.Database.MongoDB (select, (=:))

import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IterIO.Http
import Data.IterIO.Http.Support

import Hails.App
import Hails.Data.LBson (cast', ObjectId, encodeDoc)

import Control.Monad (liftM, void)

data ProjectsController = ProjectsController

contentType :: Monad m => Action t m S8.ByteString
contentType = do
  mctype <- requestHeader "accept"
  return $ fromMaybe "text/plain" mctype

instance RestController a DC ProjectsController where
  restShow _ projectName = do
    policy <- liftLIO gitstar
    uName <- fmap L8.unpack $ paramVal "user_name"
    mProj <- liftLIO $ findWhere policy $
                select [ "name" =: L8.unpack projectName
                       , "owner" =: uName ] "projects"
    with404orJust mProj $ \proj -> do
      atype <- requestHeader "accept"
      case atype of
        Just "application/bson" ->
          render "application/bson" $ encodeDoc $ toDocument proj
        _ -> renderHtml $ showProject proj

  restEdit _ projectName = do
    policy <- liftLIO gitstar
    uName <- fmap L8.unpack $ paramVal "user_name"
    mProj <- liftLIO $ findWhere policy $
                select [ "name" =: L8.unpack projectName
                       , "owner" =: uName ] "projects"
    with404orJust mProj $ \proj -> renderHtml $ editProject proj

  -- /projects/new
  restNew _ = renderHtml newProject

  restCreate _ = do
    policy <- liftLIO gitstar
    pOwner <- getHailsUser
    pName  <- fmap L8.unpack $ paramVal "name"
    pPub   <- isJust `liftM` param "public"
    pRedrs <- maybe [] fromCSList `liftM` param "readers"
    pDesc  <- fmap L8.unpack $ paramVal "description"
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
      then redirectTo "/projects/new"
           --TODO: print error "Project with this name exists"
      else do privs <- doGetPolicyPriv policy
              erf <- liftLIO $ insertRecordP privs policy proj
              case erf of
                Right r -> do
                  maybe (return ()) (\oid -> do
                   oldU <- liftLIO $ getOrCreateUser pOwner
                   let usr = oldU {userProjects = Just oid : userProjects oldU}
                   void . liftLIO $ saveRecordP privs policy usr) $ cast' r
                  redirectTo $ "/" ++ pOwner ++ "/" ++ pName
                _      -> respondStat stat500
      where projExists policy owner projName = do
              let qry = select ["name" =: projName, "owner" =: owner] "projects"
              mproj <- liftLIO $ findWhere policy qry
              return $ case mproj of
                         (Just (Project {})) -> True
                         _                   -> False

  restUpdate _ projName = do
    policy <- liftLIO gitstar
    uName <- fmap L8.unpack $ paramVal "user_name"
    projM <- liftLIO $ findWhere policy $ select [ "name" =: L8.unpack projName
                                                 , "owner" =: uName]
                                                 "projects"
    case projM of
      Just proj -> do
        pPub   <- isJust `liftM` param "public"
        pRedrs <- maybe [] fromCSList `liftM` param "readers"
        pDesc  <- fmap L8.unpack $ paramVal "description"
        pColls <- maybe [] fromCSList `liftM` param "collaborators"
        let projFinal = proj { projectDescription   = pDesc 
                             , projectCollaborators = pColls
                             , projectReaders       = if pPub then Left Public
                                                              else Right pRedrs
                             }
        privs <- doGetPolicyPriv policy
        erf <- liftLIO $ saveRecordP privs policy projFinal
        case erf of
          Right _ -> redirectTo $ "/" ++ projectOwner projFinal ++
                                  "/" ++ projectName projFinal
          _      -> respondStat stat500
      Nothing -> respond404

