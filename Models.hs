{-# LANGUAGE Safe #-}

module Models ( UserName, User(..), SSHKey(..)
              , ProjectId, Project(..), Public(..)
              , isPublic
              , projectRepository
              , projectObjId
              -- * Misc
              , module Hails.Database.MongoDB.Structured
              ) where

import Policy.Gitstar
import Data.Maybe (fromJust)
import Hails.Data.LBson (ObjectId)
import Hails.Database.MongoDB.Structured

-- | Is the project publicly readable.
isPublic :: Project -> Bool
isPublic proj = either (const True) (const False) $ projectReaders proj

-- | Project repository path.
projectRepository :: Project -> String
projectRepository proj = projectOwner proj ++ "/" ++ projectName proj ++ ".git"

-- | get Project id of an already inserted project. Error otherwise
projectObjId :: Project -> ObjectId
projectObjId = fromJust . projectId
