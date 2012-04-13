{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

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

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IterIO.Http
import Data.IterIO.Http.Support
import Data.Maybe

import Hails.App
import Hails.Data.LBson (cast', encodeDoc)

data ProjectsController = ProjectsController

instance RestController t (DCLabeled L8.ByteString) DC ProjectsController where
  restShow _ projName = do
    policy <- liftLIO gitstar
    privs <- getPolicyPrivIfUserIsGitstar policy
    uName <- getParamVal "user_name"
    mProj <- liftLIO $ findWhereP privs policy $
                select [ "name" =: L8.unpack projName
                       , "owner" =: uName ] "projects"
    with404orJust mProj $ \proj -> do
      mas <- liftLIO $ mapM (findBy policy "apps" "_id") (projectApps proj)
      let apps = map fromJust $ filter isJust mas
      atype <- requestHeader "accept"
      case atype of
        Just "application/bson" ->
          render "application/bson" $ encodeDoc $ toDocument proj
        _ -> renderHtml $ showProject proj apps
    where getPolicyPrivIfUserIsGitstar policy = do
          usr <- getHailsUser
          if usr == "gitstar" -- ssh server is making request
            then appGetPolicyPriv policy
            else return noPrivs


  restEdit _ projName = do
    policy <- liftLIO gitstar
    curUsr <- getHailsUser
    uName  <- getParamVal "user_name"
    if curUsr /= uName
      then respondStat stat403
      else do mProj <- liftLIO $ findWhere policy $
                          select [ "name" =: L8.unpack projName
                                 , "owner" =: uName ] "projects"
              with404orJust mProj $ \proj -> renderHtml $ editProject proj

  -- /projects/new
  restNew _ = renderHtml newProject

  restCreate _ = do
    policy <- liftLIO gitstar
    uName  <- getHailsUser
    ldoc   <- bodyToLDoc
    lproj  <- liftLIO $ mkProject uName ldoc
    proj   <- liftLIO $ unlabel lproj
    let pOwner = projectOwner proj
        pName  = projectName  proj

    exists <- projExists policy pOwner pName

    if exists
      then redirectBack >> flashError "Project already exists!" 
      else do erf <- liftLIO $ createProject lproj
              case erf of
                Right r -> do
                  liftLIO $ maybe (return ())
                                  (updateUserWithProjId uName) $ cast' r
                  redirectTo $ "/" ++ pOwner ++ "/" ++ pName
                  flashSuccess "Project created successfully!" 
                _      -> respondStat stat500
      where projExists policy owner projName = do
              let qry = select ["name" =: projName, "owner" =: owner] "projects"
              mproj <- liftLIO $ findWhere policy qry
              return $ case mproj of
                         (Just (Project {})) -> True
                         _                   -> False

  restUpdate _ projName = do
    uName  <- getHailsUser
    ldoc   <- bodyToLDoc
    let pName = L8.unpack projName
    res    <- liftLIO $ do lproj <- partialProjectUpdate uName pName ldoc
                           gitstarSaveLabeledRecord lproj
    either (const redirectBack)
           (const $ redirectTo $ "/" ++ uName ++ "/" ++ L8.unpack projName) res

