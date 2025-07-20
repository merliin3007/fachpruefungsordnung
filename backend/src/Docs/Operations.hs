module Docs.Operations
    ( treeWithTextVersions
    , treeVersionWithTextVersions
    , treeWithMaybeTextVersions
    , treeVersionWithMaybeTextVersions
    ) where

import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Docs.Text
    ( TextElement (textElementID)
    , TextElementID
    , TextElementVersion (TextElementVersion)
    , TextVersion
    )
import Docs.Tree
    ( Edge (..)
    , ExistingTreeVersion (ExistingTreeVersion)
    , Tree (..)
    , TreeVersion (treeVersionRoot)
    , mapMTreeVersionRoot
    , replaceTreeVersionRoot
    )

-- | Takes a tree version and emplaces concrecte text versions.
-- | The text versions are obtained via the specified getter function.
treeVersionWithTextVersions
    :: (Monad m)
    => (TextElementID -> m TextVersion)
    -- ^ (potentially effectful) function for obtaining a text version
    -> ExistingTreeVersion TextElement
    -- ^ document structre tree version
    -> m (ExistingTreeVersion TextElementVersion)
    -- ^ document structre tree version with concrete text versions
treeVersionWithTextVersions getTextVersion (ExistingTreeVersion id_ treeVersion) =
    mapMTreeVersionRoot (treeWithTextVersions getTextVersion) treeVersion
        <&> ExistingTreeVersion id_

-- | Takes a tree and emplaces concrete text versions.
-- | The text versions are obtained via the specified getter function.
treeWithTextVersions
    :: (Monad m)
    => (TextElementID -> m TextVersion)
    -- ^ (potentially effectful) function for obtaining a text version
    -> Tree TextElement
    -- ^ document structure tree
    -> m (Tree TextElementVersion)
    -- ^ document structure tree with concrete text versions
treeWithTextVersions getTextVersion = treeWithTextVersions'
  where
    treeWithTextVersions' (Leaf textElement) =
        getTextVersion (textElementID textElement)
            <&> Leaf . TextElementVersion textElement
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
    => (TextElementID -> m (Maybe TextVersion))
    -- ^ (potentially effectful) function for obtaining a text version
    -> ExistingTreeVersion TextElement
    -- ^ document structre tree version
    -> m (Maybe (ExistingTreeVersion TextElementVersion))
    -- ^ document structre tree version with concrete text versions
treeVersionWithMaybeTextVersions
    getTextVersion
    (ExistingTreeVersion id_ treeVersion) = do
        let oldRoot = treeVersionRoot treeVersion
        newRoot <- treeWithMaybeTextVersions getTextVersion oldRoot
        let newTreeVersion = replaceTreeVersionRoot <$> newRoot <*> pure treeVersion
        return $ ExistingTreeVersion id_ <$> newTreeVersion

-- | Takes a tree and emplaces concrete text versions.
-- | The text versions are obtained via the specified getter function.
-- | This function may return 'Nothing'. In such a case, the corresponding Leaf is
-- | missing in the resulting tree version.
-- | If the root is a leaf and is missing, the result will be 'Nothing'.
treeWithMaybeTextVersions
    :: (Monad m)
    => (TextElementID -> m (Maybe TextVersion))
    -- ^ (potentially effectful) function for obtaining a text version
    -> Tree TextElement
    -- ^ document structure tree
    -> m (Maybe (Tree TextElementVersion))
    -- ^ document structure tree with concrete text versions
treeWithMaybeTextVersions getTextVersion = treeWithTextVersions'
  where
    treeWithTextVersions' (Leaf textElement) =
        getTextVersion (textElementID textElement)
            <&> (Leaf . TextElementVersion textElement <$>)
    treeWithTextVersions' (Node metaData edges) =
        mapM mapEdge edges <&> Just . Node metaData . catMaybes
      where
        mapEdge (Edge label tree) =
            treeWithTextVersions' tree <&> (Edge label <$>)
