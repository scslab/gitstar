{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
-- | Basic git types. The current interface does not interpret types
-- such as dates or mime types. Inteface is based on the "git-object"
-- package.
module Gitstar.Repo.Types ( GitObject(..)
                          , GitType(..), GitMode(..)
                          , GitBlob(..)
                          , GitTree, GitTreeEntry(..)
                          , GitCommit(..)
                          , GitRef(..)
                          , GitTag(..)
                          , SHA1(..)
                          , Author(..)
                          ) where

import Data.ByteString (ByteString)
import System.Posix.Types (FileMode)

-- | SHA1 digest
newtype SHA1 = SHA1 String deriving(Eq)

instance Show SHA1 where show (SHA1 x) = x

-- | Git object types
data GitType = GtBlob   -- ^ blob
             | GtTree   -- ^ tree
             | GtCommit -- ^ commit
             | GtTag    -- ^ tag
             deriving (Eq, Show)

-- | The associated mode of a blob
data GitMode = RegularFile FileMode   -- ^ 100ugo
             | Directory              -- ^ 040000
             | SymbolicLink           -- ^ 120000
             | GitLink                -- ^ 160000
             deriving (Eq, Show)

-- | Gith author/commiter
data Author = Author { authDate  :: ByteString
                     , authName  :: ByteString
                     , authEmail :: ByteString
                     } deriving (Eq, Show)

-- | Git objects
data GitObject = GoBlob   GitBlob   -- ^ blob
               | GoTree   GitTree   -- ^ tree
               | GoCommit GitCommit -- ^ commit
               | GoTag    GitTag    -- ^ tag
               deriving (Eq, Show)

-- | Blob is simply a ByteString
data GitBlob = GitBlob { blobContent :: ByteString
                       -- ^ Actual blob
                       , blobMimeType :: String
                       } deriving (Eq, Show)


-- | A tree is a list of tree entries
type GitTree = [GitTreeEntry]

-- | A git tree object
data GitTreeEntry = GitTreeEntry { entPath :: FilePath
                                 -- ^ File name
                                 , entMode :: GitMode
                                 -- ^ Associated mode
                                 , entType :: GitType
                                 -- ^ Associated type
                                 , entSize :: Maybe Int
                                 -- ^ If the tree entry is s blob,
                                 -- size may be available
                                 , entPtr  :: SHA1
                                 -- ^ Pointer to blob or sub tree
                                 } deriving (Eq, Show)

-- | A git commit object
data GitCommit = GitCommit { cmtPtr       :: SHA1       -- ^ Pointer to tree
                           , cmtAuthor    :: Author     -- ^ Author
                           , cmtCommitter :: Author     -- ^ Commiter
                           , cmtTree      :: SHA1       -- ^ Tre
                           , cmtMessage   :: ByteString -- ^ Commit message
                           , cmtParents   :: [SHA1]     -- ^ Parents
                           } deriving (Eq, Show)

-- | A git reference
data GitRef = GitRef { refName :: FilePath
                     -- ^ Ref name
                     , refType :: GitType 
                     -- ^ Associated object's type
                     , refPtr  :: SHA1
                     -- ^ Pointer to object
                     } deriving(Eq, Show)

-- | A git tag
data GitTag = GitTag { tagPtr     :: SHA1
                     -- ^ Pointer to (commit) object
                     , tagName    :: ByteString
                     -- ^ Tagger
                     , tagMessage :: ByteString
                     -- ^ Name of tag
                     , tagAuthor  :: Author
                     -- ^ Tag author
                     } deriving (Eq, Show)
