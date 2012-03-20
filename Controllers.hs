{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module Controllers where

import Prelude hiding (lookup)

import Layouts
import Models
import Policy.Gitstar
import Views

import LIO
import LIO.DCLabel

import Data.Char (isSpace)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IterIO.Http
import Data.IterIO.Http.Support

import Hails.Data.LBson (cast', ObjectId)
import Hails.Database.MongoDB (getPolicyPriv)

import Control.Monad (liftM)

data ProjectsController = ProjectsController

instance RestController DC ProjectsController where
  restShow _ pid = do
  {-
    policy <- liftLIO gitstar
    let oid = read (L8.unpack pid) :: ObjectId
    projM <- liftLIO $ findBy policy "projects" "_id" oid
    case projM of
      Just proj -> renderHtml $ showProject proj
      Nothing   -> respond404
      -}
    respond404

  restEdit _ pid = do
    let oid = read (L8.unpack pid) :: ObjectId
    policy <- liftLIO gitstar
    projM <- liftLIO $ findBy policy "projects" "_id" oid
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
    erf <- liftLIO $ insertRecordP privs policy "projects" proj
    case erf of
      Right r -> redirectTo $ case cast' r of
        Just oid -> "/projects/" ++ show (oid :: ObjectId)
        Nothing  -> "/users/" ++ projectOwner proj ++ "/" ++ projectName proj
      _      -> respondStat stat500

  restUpdate _ pid = do
    policy <- liftLIO gitstar
    let oid = read (L8.unpack pid) :: ObjectId
    projM <- liftLIO $ findBy policy "projects" "_id" oid
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
        erf <- liftLIO $ saveRecordP privs policy "projects" projFinal
        case erf of
          Right _ -> redirectTo $ "/projects/" ++ (show . projectObjId $ projFinal)
          _      -> respondStat stat500
        case erf of
          _      -> respondStat stat500
      Nothing -> respond404

--
-- Helpers
--

getParamVal n = (L8.unpack . paramValue . fromJust) `liftM` param n
fromCSList = map (strip . L8.unpack) . L8.split ',' . paramValue
  where strip = filter (not . isSpace)

doGetPolicyPriv policy = do
  appName <- fromJust `liftM` requestHeader "x-hails-app"
  curL <- liftLIO $ getLabel
  let l = newDC (secrecy curL) (principal appName)
  liftLIO $ getPolicyPriv policy (label l)
