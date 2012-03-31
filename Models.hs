{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif

module Models ( -- * Projects
                ProjectId, Project(..), Public(..)
              , isPublic
              , projectRepository
              , projectObjId
                -- * Users
              , UserName, User(..), SSHKey(..)
              , fingerprint
              -- * Misc
              , module Hails.Database.MongoDB.Structured
              ) where

import Policy.Gitstar
import Data.Maybe (fromJust)
import Hails.Data.LBson (ObjectId, Binary(..))
import Hails.Database.MongoDB.Structured

import Hails.Crypto (md5)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

--
-- Projects related
--

-- | Is the project publicly readable.
isPublic :: Project -> Bool
isPublic proj = either (const True) (const False) $ projectReaders proj

-- | Project repository path.
projectRepository :: Project -> String
projectRepository proj = projectOwner proj ++ "/" ++ projectName proj ++ ".git"

-- | get Project id of an already inserted project. Error otherwise
projectObjId :: Project -> ObjectId
projectObjId = fromJust . projectId

--
-- Users related
--

-- | Generate the SSH fingerprint format for the 'SSHKey' based on
-- draft-ietf-secsh-fingerprint-00 (matches output from
-- ssh-keygen -lf [pubkey_file])
fingerprint :: SSHKey -> String
fingerprint key = separate . show $ md5 keyData
  where keyData = lazyfy $ B64.decodeLenient key64
        key64 = case S8.words keyVal of
                  (_:blob:_) -> blob
                  [blob]     -> blob
                  _          -> error "fingerprint: invalid key"
        keyVal = case sshKeyValue key of
                   (Binary bs) -> bs
                   _           -> S8.empty
        separate (a:b:c:xs) = a:b:':':separate (c:xs)
        separate a = a
        lazyfy = L8.pack . S8.unpack
