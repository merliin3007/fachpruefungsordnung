module Docs.Operations
    ( treeWithTextVersions
    , treeVersionWithTextVersions
    ) where

import Data.Functor ((<&>))
import Docs.Text
    ( ExistingTextVersion
    , TextElement (textElementID)
    , TextElementID
    , TextElementVersion (TextElementVersion)
    )
import Docs.Tree
    ( Edge (..)
    , ExistingTreeVersion (ExistingTreeVersion)
    , Tree (..)
    , mapMTreeVersionRoot
    )

treeVersionWithTextVersions
    :: (Monad m)
    => (TextElementID -> m ExistingTextVersion)
    -- ^ (potentially effectful) function for obtaining a text version
    -> ExistingTreeVersion TextElement
    -- ^ document structre tree version
    -> m (ExistingTreeVersion TextElementVersion)
    -- ^ document structre tree version with concrete text versions
treeVersionWithTextVersions getTextVersion (ExistingTreeVersion id_ treeVersion) =
    mapMTreeVersionRoot (treeWithTextVersions getTextVersion) treeVersion
        <&> ExistingTreeVersion id_

treeWithTextVersions
    :: (Monad m)
    => (TextElementID -> m ExistingTextVersion)
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
