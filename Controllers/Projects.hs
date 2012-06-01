{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Projects ( ProjectsController(..) ) where

import Prelude hiding (lookup)
import Control.Monad

import Layouts
import Models
import Policy.Gitstar
import Utils
import Views.Projects

import LIO
import LIO.DCLabel

import Hails.Database.MongoDB (select, look, (=:))

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IterIO.Http
import Data.IterIO.Http.Support
import Data.Maybe

import Hails.App
import Hails.Data.LBson (cast', encodeDoc)
import Hails.Database.MongoDB hiding (Action, reverse, filter, map)

data ProjectsController = ProjectsController

instance RestController t (DCLabeled L8.ByteString) DC ProjectsController where
  restIndex _ = do
    projects <- liftLIO $ do
      policy <- gitstar
      findAll policy $ select [] "projects"
    renderHtml $ listProjects projects

  restShow _ projName = do
    muser <- getHailsUser
    policy <- liftLIO gitstar
    uName <- getParamVal "user_name"
    mProj <- liftLIO $ findWhere policy $
                select [ "name" =: L8.unpack projName
                       , "owner" =: uName ] "projects"
    with404orJust mProj $ \proj -> do
      frkdP <- liftLIO $ findBy policy "projects" "_id" (projectForkedFrom proj)
      mas <- liftLIO $ mapM (findBy policy "apps" "_id") (projectApps proj)
      let apps = map fromJust $ filter isJust mas
      atype <- requestHeader "accept"
      case atype of
        Just "application/bson" ->
          render "application/bson" $ encodeDoc $ toDocument proj
        _ -> renderHtml $
              showProject muser proj apps frkdP


  restEdit _ projName = withUserOrDoAuth $ \curUsr -> do
    policy <- liftLIO gitstar
    uName  <- getParamVal "user_name"
    if curUsr /= uName
      then respondStat stat403
      else do mProj <- liftLIO $ findWhere policy $
                          select [ "name" =: L8.unpack projName
                                 , "owner" =: uName ] "projects"
              with404orJust mProj $ \proj -> renderHtml $ editProject proj

  -- /projects/new
  restNew _ = withUserOrDoAuth $ \_ -> renderHtml newProject

  -- Create a new project (from scractch or fork)
  -- TODO: mkProject may throw an exception if the document is not
  -- "well-formed". We can handle this more gracefully.
  restCreate _ = withUserOrDoAuth $ \uName -> do
    policy <- liftLIO gitstar
    ldoc   <- bodyToLDoc
    lproj  <- liftLIO $ mkProject uName ldoc
    proj   <- liftLIO $ unlabel lproj
    let pOwner = projectOwner proj
        pName  = projectName  proj
        isFork = isJust $ projectForkedFrom proj

    exists <- projExists policy pOwner pName

    if exists
      then redirectBack >> flashError "Project already exists!" 
      else do erf <- liftLIO $ createProject lproj
              case erf of
                Right r -> do
                  liftLIO $ maybe (return ())
                                  (updateUserWithProjId uName) $ cast' r
                  redirectTo $ "/" ++ pOwner ++ "/" ++ pName
                  flashSuccess $ "Project successfully " ++
                                 if isFork then "forked!" else "created!"
                _      -> respondStat stat500
      where projExists policy owner projName = do
              let qry = select ["name" =: projName, "owner" =: owner] "projects"
              mproj <- liftLIO $ findWhere policy qry
              return $ case mproj of
                         (Just (Project {})) -> True
                         _                   -> False

  restUpdate _ projName = withUserOrDoAuth $ \uName -> do
    ldoc   <- bodyToLDoc
    let pName = L8.unpack projName
    res    <- liftLIO $ do lproj <- partialProjectUpdate uName pName ldoc
                           gitstarSaveLabeledRecord lproj
    either (const redirectBack)
           (const $ redirectTo $ "/" ++ uName ++ "/" ++ L8.unpack projName) res

