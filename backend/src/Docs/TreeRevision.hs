module Docs.TreeRevision
    ( TreeRevisionID (..)
    , TreeRevision (..)
    , ExistingTreeRevision (..)
    , mapRoot
    , mapMRoot
    , replaceRoot
    , withTextRevisions
    , withMaybeTextRevisions
    ) where

import Data.Functor ((<&>))
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Int (Int32)

import Docs.TextElement (TextElement, TextElementID)
import Docs.TextRevision (TextElementRevision, TextRevision)
import Docs.Tree (Tree)
import qualified Docs.Tree as Tree

newtype TreeRevisionID = TreeRevisionID
    { unTreeRevisionID :: Int32
    }
    deriving (Eq)

data TreeRevision a = TreeRevision
    { timestamp :: UTCTime
    , author :: UUID
    , root :: Tree a
    }

mapRoot :: (Tree a -> Tree b) -> TreeRevision a -> TreeRevision b
mapRoot f treeVersion =
    TreeRevision
        { timestamp = timestamp treeVersion
        , author = author treeVersion
        , root = f $ root treeVersion
        }

replaceRoot :: Tree b -> TreeRevision a -> TreeRevision b
replaceRoot new = mapRoot $ const new

mapMRoot
    :: (Monad m)
    => (Tree a -> m (Tree b))
    -> TreeRevision a
    -> m (TreeRevision b)
mapMRoot f treeVersion = do
    new <- f $ root treeVersion
    return $ replaceRoot new treeVersion

data ExistingTreeRevision a
    = ExistingTreeRevision
        TreeRevisionID
        (TreeRevision a)

instance Functor TreeRevision where
    fmap f treeVersion =
        TreeRevision
            { timestamp = timestamp treeVersion
            , author = author treeVersion
            , root = f <$> root treeVersion
            }

instance Functor ExistingTreeRevision where
    fmap f (ExistingTreeRevision id_ treeVersion) =
        ExistingTreeRevision id_ $ f <$> treeVersion

-- | Takes a tree revision and emplaces concrecte text revisions.
-- | The text revisions are obtained via the specified getter function.
withTextRevisions
    :: (Monad m)
    => (TextElementID -> m TextRevision)
    -- ^ (potentially effectful) function for obtaining a text revision
    -> ExistingTreeRevision TextElement
    -- ^ document structre tree revision
    -> m (ExistingTreeRevision TextElementRevision)
    -- ^ document structre tree revision with concrete text revision
withTextRevisions getTextRevision (ExistingTreeRevision id_ treeVersion) =
    mapMRoot (Tree.withTextRevisions getTextRevision) treeVersion
        <&> ExistingTreeRevision id_

-- | Takes a tree revision and emplaces concrecte text revisions.
-- | The text revisions are obtained via the specified getter function.
-- | This function may return 'Nothing'. In such a case, the corresponding Leaf is
-- | missing in the tree of the resulting tree revision.
-- | If the root is a leaf and is missing, the result will be 'Nothing'.
withMaybeTextRevisions
    :: (Monad m)
    => (TextElementID -> m (Maybe TextRevision))
    -- ^ (potentially effectful) function for obtaining a text revision
    -> ExistingTreeRevision TextElement
    -- ^ document structre tree revision
    -> m (Maybe (ExistingTreeRevision TextElementRevision))
    -- ^ document structre tree revision with concrete text revision
withMaybeTextRevisions getTextRevision (ExistingTreeRevision id_ treeVersion) = do
    let oldRoot = root treeVersion
    newRoot <- Tree.withMaybeTextRevisions getTextRevision oldRoot
    let newTreeVersion = replaceRoot <$> newRoot <*> pure treeVersion
    return $ ExistingTreeRevision id_ <$> newTreeVersion
