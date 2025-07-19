module Docs.Operations
    ( treeWithTextVersions
    ) where

import Data.Functor ((<&>))
import Docs.Text
    ( ExistingTextVersion
    , TextElement (textElementID)
    , TextElementID
    , TextElementVersion (TextElementVersion)
    )
import Docs.Tree (Edge (..), Tree (..))

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
