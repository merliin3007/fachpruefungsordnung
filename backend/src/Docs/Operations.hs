module Docs.Operations
    ( treeWithTextVersions
    , treeVersionWithTextVersions
    , treeWithMaybeTextVersions
    , treeVersionWithMaybeTextVersions
    ) where

import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Docs.TextElement
    ( TextElement
    , TextElementID
    )
import qualified Docs.TextElement as TextElement
import Docs.TextRevision
    ( TextElementRevision (TextElementRevision)
    , TextRevision
    )
import Docs.Tree
    ( Edge (..)
    , Tree (..)
    )
import Docs.TreeRevision
    ( ExistingTreeRevision (ExistingTreeRevision)
    )
import qualified Docs.TreeRevision as TreeRevision

-- | Takes a tree version and emplaces concrecte text versions.
-- | The text versions are obtained via the specified getter function.
treeVersionWithTextVersions
    :: (Monad m)
    => (TextElementID -> m TextRevision)
    -- ^ (potentially effectful) function for obtaining a text version
    -> ExistingTreeRevision TextElement
    -- ^ document structre tree version
    -> m (ExistingTreeRevision TextElementRevision)
    -- ^ document structre tree version with concrete text versions
treeVersionWithTextVersions getTextVersion (ExistingTreeRevision id_ treeVersion) =
    TreeRevision.mapMRoot (treeWithTextVersions getTextVersion) treeVersion
        <&> ExistingTreeRevision id_

-- | Takes a tree and emplaces concrete text versions.
-- | The text versions are obtained via the specified getter function.
treeWithTextVersions
    :: (Monad m)
    => (TextElementID -> m TextRevision)
    -- ^ (potentially effectful) function for obtaining a text version
    -> Tree TextElement
    -- ^ document structure tree
    -> m (Tree TextElementRevision)
    -- ^ document structure tree with concrete text versions
treeWithTextVersions getTextVersion = treeWithTextVersions'
  where
    treeWithTextVersions' (Leaf textElement) =
        getTextVersion (TextElement.identifier textElement)
            <&> Leaf . TextElementRevision textElement
    treeWithTextVersions' (Node metaData edges) =
        mapM mapEdge edges <&> Node metaData
      where
        mapEdge (Edge label tree) =
            treeWithTextVersions' tree <&> Edge label

-- | Takes a tree version and emplaces concrecte text versions.
-- | The text versions are obtained via the specified getter function.
-- | This function may return 'Nothing'. In such a case, the corresponding Leaf is
-- | missing in the tree of the resulting tree version.
-- | If the root is a leaf and is missing, the result will be 'Nothing'.
treeVersionWithMaybeTextVersions
    :: (Monad m)
    => (TextElementID -> m (Maybe TextRevision))
    -- ^ (potentially effectful) function for obtaining a text version
    -> ExistingTreeRevision TextElement
    -- ^ document structre tree version
    -> m (Maybe (ExistingTreeRevision TextElementRevision))
    -- ^ document structre tree version with concrete text versions
treeVersionWithMaybeTextVersions
    getTextVersion
    (ExistingTreeRevision id_ treeVersion) = do
        let oldRoot = TreeRevision.root treeVersion
        newRoot <- treeWithMaybeTextVersions getTextVersion oldRoot
        let newTreeVersion = TreeRevision.replaceRoot <$> newRoot <*> pure treeVersion
        return $ ExistingTreeRevision id_ <$> newTreeVersion

-- | Takes a tree and emplaces concrete text versions.
-- | The text versions are obtained via the specified getter function.
-- | This function may return 'Nothing'. In such a case, the corresponding Leaf is
-- | missing in the resulting tree version.
-- | If the root is a leaf and is missing, the result will be 'Nothing'.
treeWithMaybeTextVersions
    :: (Monad m)
    => (TextElementID -> m (Maybe TextRevision))
    -- ^ (potentially effectful) function for obtaining a text version
    -> Tree TextElement
    -- ^ document structure tree
    -> m (Maybe (Tree TextElementRevision))
    -- ^ document structure tree with concrete text versions
treeWithMaybeTextVersions getTextVersion = treeWithTextVersions'
  where
    treeWithTextVersions' (Leaf textElement) =
        getTextVersion (TextElement.identifier textElement)
            <&> (Leaf . TextElementRevision textElement <$>)
    treeWithTextVersions' (Node metaData edges) =
        mapM mapEdge edges <&> Just . Node metaData . catMaybes
      where
        mapEdge (Edge label tree) =
            treeWithTextVersions' tree <&> (Edge label <$>)
