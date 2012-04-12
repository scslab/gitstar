{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
-- | Basic git types. The current interface does not interpret types
-- such as dates or mime types. Inteface is based on the "git-object"
-- package.
module Gitstar.Repo.Types ( Repo(..)
                           -- * Git object specific
                          , GitObject(..)
                          , GitType(..), GitMode(..)
                          , GitBlob(..)
                          , GitTree, GitTreeEntry(..)
                          , GitCommit(..)
                          , GitRef(..)
                          , GitTag(..)
                          , SHA1(..)
                          , Author(..)
                          -- * Git diff, blame, stats
                          , GitDiff(..), PathChanges(..), DiffPath(..)
                          , GitStats(..), GitFileStat(..)
                          , GitBlame, GitBlameLine(..)
                          ) where

import Data.ByteString (ByteString)
import System.Posix.Types (FileMode)

data Repo = Repo { repoName  :: String -- ^ Project name
                 , repoOwner :: String -- ^ Project owner
                 } deriving (Eq, Show)

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
                       -- ^ Base64-encoded blob
                       , blobMimeType :: String
                       -- ^ Mime type from git
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


--
-- Git diff, blame, stats
--

-- | Transformation to a file pat
data PathChanges = DeletedFile          -- ^ File deleted
                 | NewFile              -- ^ File is new
                 deriving (Eq, Show)

-- | A path and any recorded transformations
data DiffPath = DiffPath { dpathName    :: FilePath
                         -- ^ Name of file
                         , dpathChanges :: Maybe PathChanges
                         -- ^ Path transformations
                         } deriving (Eq, Show)

-- | A diff object contains the path, similarity index, and actual
-- changes.
data GitDiff = GitDiff { diffPath :: DiffPath        -- ^ Path of file
                       , diffSimilarlityIndex :: Int -- ^ Similarity index
                       , diffContent :: ByteString   -- ^ Actual diff
                       } deriving (Eq, Show)


-- | Git commit status
data GitStats = GitStats { statCommit    :: SHA1       -- ^ Commit id
                         , statFiles     :: [GitFileStat] -- ^ Actual stat files
                         , statAdditions :: Int        -- ^ Number of additions
                         , statDeletions :: Int        -- ^ Number of deletions
                         , statTotal     :: Int        -- ^ Number of changes
                         } deriving (Eq, Show)

-- | Status of a file
data GitFileStat = GitFileStat { fstatPath :: FilePath -- ^ File name
                               , fstatAdditions :: Int -- ^ Number of additions
                               , fstatDeletions :: Int -- ^ Number of deletions
                               , fstatTotal     :: Int -- ^ Number of changes
                               } deriving (Eq, Show)

-- | A blame is a list of line blames
type GitBlame = [GitBlameLine]

-- | Blame data for a line of a file
data GitBlameLine = GitBlameLine { blmLineNr    :: Int  -- ^ Current line number
                                 , blmOldLineNr :: Int  -- ^ Old line umber
                                 , blmLine      :: ByteString -- ^ Line content
                                 , blmCommit    :: SHA1 -- ^ Reponsible commit
                                 } deriving (Eq, Show)
